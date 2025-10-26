# bash
python3 - <<'PY'
points=[742,743,771,776,778,794,798,799,800,805,809,810,815,865]
import unicodedata
for p in points:
    ch = chr(p)
    try:
        name = unicodedata.name(ch)
    except ValueError:
        name = "<no name>"
    print(f"U+{p:04X}\t{p}\t{ch}\t{name}")
PY