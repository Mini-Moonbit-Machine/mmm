assignment = 'v15<-a0;v10<-a1;v12<-a0;v2<-a0;v7<-a1;v3<-a1;v6<-a1;v5<-a1;v25<-a1;v11<-a0;v14<-a0;v27<-a1;v8<-a1;v13<-a0;v9<-a1;v4<-a2;v19<-a1;v29<-a0;v17<-a1;v23<-a1;'
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