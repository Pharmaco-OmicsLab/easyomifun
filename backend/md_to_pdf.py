#!/usr/bin/env python3
"""
EasyOmiFun Markdown to PDF Compiler
Ultra-fast, platform-independent (macOS arm64/x64, Linux, Windows) Markdown-to-PDF
rendering engine designed for EasyOmiFun analysis reports.
"""

import sys
import os
import re
import html

try:
    from fpdf import FPDF
except ImportError:
    print("[ERROR] fpdf2 is not installed in the Python environment.", file=sys.stderr)
    sys.exit(1)


class EasyOmiFunReportPDF(FPDF):
    def __init__(self, title="EasyOmiFun Analysis Report"):
        super().__init__(orientation="P", unit="mm", format="A4")
        self.report_title = title
        self.set_auto_page_break(auto=True, margin=18)
        self.set_margins(18, 18, 18)

    def header(self):
        self.set_font("Helvetica", "B", 8)
        self.set_text_color(100, 116, 139) # slate-500
        self.cell(0, 6, self.report_title, align="R", new_x="LMARGIN", new_y="NEXT")
        self.set_draw_color(226, 232, 240) # slate-200
        self.set_line_width(0.3)
        self.line(18, 14, self.w - 18, 14)
        self.ln(3)

    def footer(self):
        self.set_y(-14)
        self.set_font("Helvetica", "", 8)
        self.set_text_color(148, 163, 184) # slate-400
        self.cell(0, 6, f"Page {self.page_no()}", align="C")


def clean_text(text: str) -> str:
    """Normalize markdown entities and unsupported unicode characters for standard PDF fonts."""
    t = text
    # Common HTML entities
    t = t.replace("&alpha;", "alpha")
    t = t.replace("&beta;", "beta")
    t = t.replace("&gamma;", "gamma")
    t = t.replace("&plusmn;", "+/-")
    t = t.replace("&times;", "x")
    t = t.replace("&le;", "<=")
    t = t.replace("&ge;", ">=")
    t = t.replace("&ne;", "!=")
    t = t.replace("&approx;", "~")
    t = t.replace("±", "+/-")
    t = t.replace("→", "->")
    t = t.replace("←", "<-")
    t = t.replace("•", "-")
    t = t.replace("—", "--")
    t = t.replace("–", "-")
    t = t.replace("“", '"').replace("”", '"')
    t = t.replace("‘", "'").replace("’", "'")
    t = html.unescape(t)
    return t.encode("latin-1", "replace").decode("latin-1")


def strip_inline_formatting(text: str) -> str:
    """Strip bold, italic, code ticks while preserving the text."""
    t = re.sub(r"\*\*([^*]+)\*\*", r"\1", text)
    t = re.sub(r"\*([^*]+)\*", r"\1", t)
    t = re.sub(r"`([^`]+)`", r"\1", t)
    t = re.sub(r"_([^_]+)_", r"\1", t)
    return t


def render_markdown_to_pdf(md_path: str, pdf_path: str, title: str = "EasyOmiFun Analysis Report"):
    if not os.path.exists(md_path):
        raise FileNotFoundError(f"Markdown file not found: {md_path}")

    with open(md_path, "r", encoding="utf-8", errors="replace") as f:
        lines = f.readlines()

    # Extract title from first # heading if present
    extracted_title = title
    for l in lines:
        s = l.strip()
        if s.startswith("# "):
            extracted_title = clean_text(s[2:].strip())
            break

    pdf = EasyOmiFunReportPDF(title=extracted_title)
    pdf.add_page()

    in_table = False
    table_rows = []

    def flush_table():
        nonlocal in_table, table_rows
        if not in_table or not table_rows:
            in_table = False
            table_rows = []
            return

        n_cols = max(len(r) for r in table_rows)
        if n_cols == 0:
            in_table = False
            table_rows = []
            return

        col_w = pdf.epw / max(1, n_cols)
        
        # Check if table fits on current page, if not, add page
        est_height = len(table_rows) * 6 + 6
        if pdf.get_y() + est_height > pdf.h - pdf.b_margin:
            pdf.add_page()

        # Header Row
        pdf.set_font("Helvetica", "B", 8)
        pdf.set_fill_color(241, 245, 249) # slate-100
        pdf.set_text_color(30, 41, 59)     # slate-800
        pdf.set_draw_color(203, 213, 225)  # slate-300
        pdf.set_line_width(0.2)

        for c_idx in range(n_cols):
            val = table_rows[0][c_idx] if c_idx < len(table_rows[0]) else ""
            clean_val = clean_text(strip_inline_formatting(val))
            pdf.cell(col_w, 6.5, clean_val[:32], border=1, fill=True, align="C")
        pdf.ln(6.5)

        # Data Rows
        pdf.set_font("Helvetica", "", 8)
        pdf.set_text_color(51, 65, 85) # slate-700
        for r_idx, row in enumerate(table_rows[1:]):
            fill_bg = (r_idx % 2 == 1)
            if fill_bg:
                pdf.set_fill_color(248, 250, 252)
            else:
                pdf.set_fill_color(255, 255, 255)

            for c_idx in range(n_cols):
                val = row[c_idx] if c_idx < len(row) else ""
                clean_val = clean_text(strip_inline_formatting(val))
                pdf.cell(col_w, 5.8, clean_val[:35], border=1, fill=fill_bg, align="C")
            pdf.ln(5.8)

        pdf.ln(3)
        table_rows = []
        in_table = False

    for raw_line in lines:
        line = raw_line.strip()

        # Detect Markdown Tables
        if line.startswith("|") and line.endswith("|"):
            cols = [c.strip() for c in line.split("|")[1:-1]]
            if all(re.match(r"^:?-+:?$", c) for c in cols):
                continue # Skip markdown separator row |---|---|
            table_rows.append(cols)
            in_table = True
            continue
        elif in_table:
            flush_table()

        if not line:
            pdf.ln(2)
            continue

        clean_l = clean_text(line)

        # Headings
        if clean_l.startswith("# "):
            pdf.ln(4)
            pdf.set_font("Helvetica", "B", 16)
            pdf.set_text_color(15, 23, 42) # slate-900
            pdf.multi_cell(pdf.epw, 8, clean_l[2:])
            pdf.ln(1)
        elif clean_l.startswith("## "):
            pdf.ln(3)
            pdf.set_font("Helvetica", "B", 12.5)
            pdf.set_text_color(30, 41, 59) # slate-800
            pdf.multi_cell(pdf.epw, 7, clean_l[3:])
            pdf.ln(1)
        elif clean_l.startswith("### "):
            pdf.ln(2.5)
            pdf.set_font("Helvetica", "B", 10.5)
            pdf.set_text_color(51, 65, 85) # slate-700
            pdf.multi_cell(pdf.epw, 6, clean_l[4:])
            pdf.ln(1)
        elif clean_l.startswith("#### "):
            pdf.ln(1.5)
            pdf.set_font("Helvetica", "B", 9.5)
            pdf.set_text_color(71, 85, 105) # slate-600
            pdf.multi_cell(pdf.epw, 5.2, clean_l[5:])
        elif clean_l.startswith("---") or clean_l.startswith("***"):
            pdf.ln(2)
            pdf.set_draw_color(226, 232, 240)
            pdf.set_line_width(0.3)
            pdf.line(pdf.l_margin, pdf.get_y(), pdf.w - pdf.r_margin, pdf.get_y())
            pdf.ln(3)
        elif clean_l.startswith(">"):
            # Blockquotes / Callout notes
            callout_text = clean_l.lstrip("> ").strip()
            callout_text = strip_inline_formatting(callout_text)
            pdf.set_fill_color(248, 250, 252) # slate-50
            pdf.set_draw_color(59, 130, 246)  # blue-500
            pdf.set_line_width(0.8)
            pdf.set_font("Helvetica", "I", 8.5)
            pdf.set_text_color(71, 85, 105)

            y_start = pdf.get_y()
            pdf.set_x(pdf.l_margin + 3)
            pdf.multi_cell(pdf.epw - 6, 4.8, callout_text, fill=True)
            y_end = pdf.get_y()
            pdf.line(pdf.l_margin, y_start, pdf.l_margin, y_end)
            pdf.ln(2)
        elif clean_l.startswith("- ") or clean_l.startswith("* "):
            # Bullet point (Top level)
            pdf.set_font("Helvetica", "", 9)
            pdf.set_text_color(51, 65, 85)
            bullet_text = strip_inline_formatting(clean_l[2:])
            pdf.set_x(pdf.l_margin + 2)
            pdf.multi_cell(pdf.epw - 4, 4.8, f"-  {bullet_text}")
        elif raw_line.startswith("    - ") or raw_line.startswith("  - ") or raw_line.startswith("    * ") or raw_line.startswith("  * "):
            # Nested sub-bullet point
            pdf.set_font("Helvetica", "", 8.5)
            pdf.set_text_color(71, 85, 105)
            sub_text = strip_inline_formatting(clean_l.lstrip("- *").strip())
            indent = 6 if raw_line.startswith("  - ") or raw_line.startswith("  * ") else 10
            pdf.set_x(pdf.l_margin + indent)
            pdf.multi_cell(pdf.epw - indent - 2, 4.4, f"-  {sub_text}")
        else:
            # Regular text paragraph
            pdf.set_font("Helvetica", "", 9)
            pdf.set_text_color(51, 65, 85)
            plain_text = strip_inline_formatting(clean_l)
            pdf.multi_cell(pdf.epw, 4.8, plain_text)

    # Flush any remaining table at the end
    flush_table()

    # Ensure parent output directory exists
    parent_dir = os.path.dirname(os.path.abspath(pdf_path))
    if parent_dir:
        os.makedirs(parent_dir, exist_ok=True)
    pdf.output(pdf_path)


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python md_to_pdf.py <input_md_file> <output_pdf_file> [report_title]")
        sys.exit(1)

    in_md = sys.argv[1]
    out_pdf = sys.argv[2]
    rep_title = sys.argv[3] if len(sys.argv) > 3 else "EasyOmiFun Analysis Report"

    try:
        render_markdown_to_pdf(in_md, out_pdf, rep_title)
        if os.path.exists(out_pdf) and os.path.getsize(out_pdf) > 0:
            sys.exit(0)
        else:
            print("[ERROR] Output PDF was not generated or is empty.", file=sys.stderr)
            sys.exit(1)
    except Exception as e:
        print(f"[ERROR] Failed to compile Markdown to PDF: {e}", file=sys.stderr)
        sys.exit(1)
