# find test -type f -name '*.hs' -print0 | while IFS= read -r -d '' f; do
#   mn=$(printf '%s' "$f" | sed -e 's#^test/##' -e 's#/#.#g' -e 's#\.hs$##')
#   echo "== $f -> module $mn (tests) where =="
#   awk '/^module /{print "  CURRENT: " $0; exit}' "$f" || true
# done

find test -type f -name '*.hs' -print0 | while IFS= read -r -d '' f; do
  mn=$(printf '%s' "$f" | sed -e 's#^test/##' -e 's#/#.#g' -e 's#\.hs$##')
  tmp=$(mktemp)
  awk -v mn="$mn" 'BEGIN{repl=0}
    { if(!repl && $1=="module") { print "module " mn " (tests) where"; repl=1; next } print }
    END{ if(repl==0){ print "/* no module line found */" > "/dev/stderr" } }' "$f" > "$tmp" && mv "$tmp" "$f"
done