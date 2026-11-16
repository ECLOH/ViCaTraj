# name=xml_to_parquet_wide_direct.py
#!/usr/bin/env python3
"""
xml_to_parquet_wide_direct.py
Usage:
  python xml_to_parquet_wide_direct.py input.xml out_parquet_dir [RecordLocalName] [batch_size]

Produces part-*.parquet files in out_parquet_dir with wide columns (keys and key_1, key_2, ...).
"""
import sys, os, re
from lxml import etree
import pyarrow as pa
import pyarrow.parquet as pq

PIPE_RE = re.compile(r'\|')

def flatten_to_lists(elem, sep="."):
    out = {}
    def rel_path(base, node):
        parts = []
        cur = node
        while cur is not None and cur is not base:
            parts.append(etree.QName(cur.tag).localname)
            cur = cur.getparent()
        parts.reverse()
        return sep.join(parts)
    for node in elem.iter():
        if not isinstance(node.tag, str):
            continue
        p = rel_path(elem, node)
        for k, v in node.attrib.items():
            key = f"{p}@{k}" if p else f"@{k}"
            out.setdefault(key, []).append(v)
        txt = (node.text or "").strip()
        if txt:
            key = p if p else etree.QName(node.tag).localname
            out.setdefault(key, []).append(txt)
    return out

def normalize_value_to_list(v):
    if v is None:
        return []
    if isinstance(v, list):
        return v
    if isinstance(v, str):
        if '|' in v:
            parts = [p for p in PIPE_RE.split(v) if p != ""]
            return parts if parts else [v]
        return [v]
    return [str(v)]

def record_to_wide(rec):
    out = {}
    for k, v in rec.items():
        lst = normalize_value_to_list(v)
        if len(lst) == 0:
            out[k] = None
        elif len(lst) == 1:
            out[k] = lst[0]
        else:
            for i, val in enumerate(lst, 1):
                out[f"{k}_{i}"] = val
    return out

def xml_to_parquet_wide(xml_path, out_dir, record_localname="InfosFoyerRSA", batch_size=3000):
    os.makedirs(out_dir, exist_ok=True)
    start_re = re.compile(r'<(?:[A-Za-z0-9_:-]+:)?' + re.escape(record_localname) + r'\b', re.IGNORECASE)
    end_re = re.compile(r'</(?:[A-Za-z0-9_:-]+:)?' + re.escape(record_localname) + r'\s*>', re.IGNORECASE)

    in_frag = False
    buf = []
    total = 0
    written = 0
    batch = []
    seen_cols = []
    seen_set = set()
    part_index = 0

    with open(xml_path, "r", encoding="utf-8", errors="ignore") as f:
        for lineno, line in enumerate(f, 1):
            if not in_frag:
                if start_re.search(line):
                    in_frag = True
                    buf = [line]
            else:
                buf.append(line)
            if in_frag and end_re.search(line):
                fragment = "".join(buf)
                total += 1
                try:
                    el = etree.fromstring(fragment.encode("utf-8"))
                    rec_lists = flatten_to_lists(el)
                    rec_lists["_node"] = record_localname
                    wide = record_to_wide(rec_lists)
                    for c in wide.keys():
                        if c not in seen_set:
                            seen_set.add(c); seen_cols.append(c)
                    batch.append(wide)
                except Exception as e:
                    err = {"_node": record_localname, "_parse_error": str(e), "_raw_head": fragment[:2000]}
                    for c in err.keys():
                        if c not in seen_set:
                            seen_set.add(c); seen_cols.append(c)
                    batch.append(err)
                in_frag = False
                buf = []
            if len(batch) >= batch_size:
                norm = [{k: r.get(k, None) for k in seen_cols} for r in batch]
                table = pa.Table.from_pylist(norm)
                pq.write_table(table, os.path.join(out_dir, f"part-{part_index:06d}.parquet"))
                part_index += 1
                written += len(batch)
                batch = []

    if batch:
        norm = [{k: r.get(k, None) for k in seen_cols} for r in batch]
        table = pa.Table.from_pylist(norm)
        pq.write_table(table, os.path.join(out_dir, f"part-{part_index:06d}.parquet"))
        written += len(batch)
        part_index += 1

    print(f"Finished. Fragments seen: {total}, records written: {written}, parts: {part_index}")
    print(f"Example columns (first 80): {seen_cols[:80]}")
    return {"fragments": total, "written": written, "parts": part_index, "columns_sample": seen_cols[:80]}

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python xml_to_parquet_wide_direct.py input.xml out_parquet_dir [RecordLocalName] [batch_size]")
        sys.exit(1)
    xml = sys.argv[1]
    outdir = sys.argv[2]
    tag = sys.argv[3] if len(sys.argv)>3 else "InfosFoyerRSA"
    bs = int(sys.argv[4]) if len(sys.argv)>4 else 3000
    xml_to_parquet_wide(xml, outdir, record_localname=tag, batch_size=bs)