python3 - <<'PY'
import unicodedata, pathlib, difflib
p=pathlib.Path('data/rules/ProtoDoll.lsc')
orig=p.read_text(encoding='utf-8')
norm=unicodedata.normalize('NFC', orig)
if orig==norm:
    print("Already NFC")
else:
    for i,(o,n) in enumerate(zip(orig.splitlines(), norm.splitlines()),1):
        if o!=n:
            print("LINE", i)
            print("  BEFORE:", [ord(c) for c in o])
            print("  AFTER: ", [ord(c) for c in n])
            print("---")
    # show a small unified diff
    for line in difflib.unified_diff(orig.splitlines(), norm.splitlines(), lineterm=''):
        print(line)
PY