assignment = 'v3<-a3;v10<-a0;v12<-a0;v27<-a0;v7<-a1;v15<-a0;v6<-a4;v5<-a0;v25<-a0;v11<-a0;v14<-a1;v2<-a2;v8<-a1;v13<-a0;v9<-a0;v4<-a2;v19<-a0;v29<-a2;v17<-a1;v23<-a0;'
asm = '''
'''

def do_subst(asgn):
  asgns = asgn.split(';')
  subst = {}
  for s in asgns:
    if s == '':
      continue
    x, y = s.split('<-')
    subst[x] = y
  head = ''
  for k, v in subst.items():
    head += f'#define {k} {v}\n'
  print(head + asm)

do_subst(assignment)