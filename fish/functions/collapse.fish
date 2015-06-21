function collapse -d "Collapse multiple spaces into single spaces"
  sed -e 's/  */ /g'
end
