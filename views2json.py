import sys
import yaml
import json

def fixView(key, value):
  return [key, fixParts(value)]

def fixParts(parts):
  results=[]
  for part in parts:
    results.append(fixPart(part))
  return results

def fixPart(part):
  partType=part[0]
  partValue=part[1]
  if partType=='func':
    return {'FuncView': partValue}
  elif partType=='block':
    return {'BlockView': partValue}
  elif partType=='symref':
    return {'SymrefView': partValue}
  elif partType=='symdef':
    return {'SymdefView': partValue}
  elif partType=='sugar':
    return {'SugarView': partValue}

yfile=sys.argv[1]
jfile=sys.argv[2]

yf=open(yfile, 'r')
jf=open(jfile, 'w')

o=yaml.load(yf)
yf.close()

o2=[]
for key in o:
  value=o[key]
  if type(value)==dict and 'view' in value:
    o2.append(fixView(key, value['view']))

jf.write(json.dumps(o2))
jf.close()
