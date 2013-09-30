import sys
import yaml
import json

def fixValue(key, value):
  return [key, fixBlock(value)]

def fixBlock(value):
  params=value['params']
  code=value['code']
  return [params, fixCode(code)]

def fixCode(code):
  ftype=code[0]
  fval=code[1]
  if ftype=='func':
    return {"FunctionExpression":fixCall(fval)}
  elif ftype=='literal':
    return {"LiteralExpression":fixLiteral(fval)}
  elif ftype=='symref':
    return {"SymrefExpression":fval}
  elif ftype=='block':
    return {"BlockExpression":fixBlock(fval)}
  elif ftype=='symdef':
    return {"SymdefExpression":fval}
  else:
    print 'Error 1'
    print ftype

def fixCall(code):
  fname=code[0]
  fargs=code[1]
  return [fname, fixArgs(fargs)]

def fixArgs(args):
  result=[]
  for arg in args:
    result.append(fixCode(arg))
  return result

def fixLiteral(literal):
  if type(literal)==int:
    return {"IntegerLiteral":literal}
  elif type(literal)==str:
    return {"StringLiteral":literal}
  elif type(literal)==bool:
    return {"BooleanLiteral":literal}
  elif type(literal)==float:
    return {"FloatLiteral":literal}
  elif type(literal)==list:
    return {"ListLiteral":fixList(literal)}
  else:
    print 'Error 2'
    print literal

def fixList(l):
  result=[]
  for item in l:
    val=item[1]
    result.append(fixLiteral(val))
  return result

yfile=sys.argv[1]
jfile=sys.argv[2]

yf=open(yfile, 'r')
jf=open(jfile, 'w')

o=yaml.load(yf)
yf.close()

bad=[]
#bad=["testIf2","scalelist","sqrtiter","testWith2","sum","fib","abs","testWith","factorial","testEval","testIf"]

max=1000
count=0
o2=[]
for key in o:
  value=o[key]
  if count<max:
    if key in bad:
      print 'Skipping', key
    else:
      print 'Good:', key
      o2.append(fixValue(key, value))
      count=count+1

jf.write(json.dumps(o2))
jf.close()
