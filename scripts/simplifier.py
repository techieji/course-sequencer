import json

with open('omnibus.json') as f:
    data = json.load(f)

real_fields = ['subject', 'catalogNbr', 'titleLong', 'crseAttrValueGroups', 'description',
               'catalogPrereq', 'catalogCoreq']

real_fields = ['subject', 'catalogNbr', 'titleLong']

simplified_data = [{k: x[k] if x[k] is not None else "" for k in real_fields} for x in data]
with open('simplified.json', 'w') as f:
    json.dump(simplified_data, f)
