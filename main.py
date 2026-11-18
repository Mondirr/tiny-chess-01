import json,os,urllib.request as R
U=os.getenv("MAKEFUN_ENV_URL")
def post(p,b):
 with R.urlopen(R.Request(U+p,json.dumps(b).encode(),{"content-type":"application/json"}))as r:return json.load(r)
V=[0,100,320,330,500,900,0]
KN=[-33,-31,-18,-14,14,18,31,33]
DI=[-17,-15,15,17];OR=[-16,-1,1,16];KI=DI+OR
N=[0]
def S(s):return"abcdefgh"[s&7]+chr((s>>4)+49)
def parse(f):
 b=[0]*128;p=f.split();r=7;c=0
 for ch in p[0]:
  if ch=="/":r-=1;c=0
  elif ch.isdigit():c+=int(ch)
  else:b[r*16+c]=("PNBRQK".index(ch.upper())+1)*(ch<'a' or-1);c+=1
 t=1-(p[1]=="b")*2
 k=sum(1<<i for i,q in enumerate("KQkq")if q in p[2])
 e=-1
 if p[3]!="-":e=ord(p[3][0])-97+(int(p[3][1])-1)*16
 return b,t,k,e
def att(b,s,y):
 for d in(15,17):
  if not(s-d*y)&136 and b[s-d*y]==y:return True
 for ds,pc in[(KN,2),(KI,6)]:
  for d in ds:
   a=s+d
   if a&136==0 and b[a]==pc*y:return True
 for ds,pc in[(DI,3),(OR,4)]:
  for d in ds:
   a=s+d
   while a&136==0:
    x=b[a]
    if x and x in(pc*y,5*y):return True
    if x:break
    a+=d
 return False
def apply(b,m,t,k,e):
 f,o,pr=m;n=b[:];pc=n[f];n[f]=0;n[o]=pr*t or pc
 if abs(pc)==1 and o==e and b[o]==0:n[o-16*t]=0
 if abs(pc)==6 and abs(o-f)==2:r=o+(1,-2)[o<f];q=o+(-1,1)[o<f];n[q]=n[r];n[r]=0
 nk=k
 for sq,bit in((0,2),(7,1),(112,8),(119,4),(4,3),(116,12)):
  if sq in(f,o):nk&=~bit
 ne=((f+o)//2,-1)[abs(pc)!=1 or abs(o-f)!=32]
 return n,nk,ne
def gen(b,t,k,e):
 ms=[]
 for s,p in enumerate(b):
  if not p or(p>0)!=(t>0):continue
  a=abs(p)
  if a==1:
   d=16*t;ts=[]
   o=s+d
   if o&136==0 and not b[o]:
    ts.append(o)
    if(s>>4)==(1,6)[t<0]and not b[s+2*d]:ts.append(s+2*d)
   for dd in(d-1,d+1):
    o=s+dd
    if o&136==0 and(b[o]and(b[o]>0)!=(t>0)or o==e):ts.append(o)
   for o in ts:
    if(o>>4)==(7,0)[t<0]:
     for pr in(5,4,3,2):ms.append((s,o,pr))
    else:ms.append((s,o,0))
  elif a in(2,6):
   for d in(KI,KN)[a==2]:
    o=s+d
    if o&136==0 and(not b[o]or(b[o]>0)!=(t>0)):ms.append((s,o,0))
   if a==6:
    for ks,bit,em,sf,rk,tg in[(4,1,(5,6),(4,5,6),7,6),(4,2,(3,2,1),(4,3,2),0,2),(116,4,(117,118),(116,117,118),119,118),(116,8,(115,114,113),(116,115,114),112,114)]:
     if s==ks and k&bit and b[rk]==4*t and all(not b[q]for q in em)and all(not att(b,q,-t)for q in sf):ms.append((ks,tg,0))
  else:
   ds=(DI,OR,KI)[a-3]
   for d in ds:
    o=s+d
    while o&136==0:
     x=b[o]
     if not x or(x>0)!=(t>0):ms.append((s,o,0))
     if x:break
     o+=d
 return[m for m in ms if not att((n:=apply(b,m,t,k,e)[0]),n.index(6*t),-t)]
def ev(b,t):
 s=0
 for q,p in enumerate(b):
  if p:
   a=abs(p);w=p>0;g=(-1,1)[w];r,f=q>>4,q&7
   s+=g*(V[a]+(6*(7-r,r)[w]if a==1 else 12*(a in(2,3)and 2<=f<=5 and 2<=r<=5)))
 return s*t
def cv(b,m,e):return V[abs(b[m[1]])]or 100*(abs(b[m[0]])<2 and m[1]==e)
def search(b,t,k,e,d,al,be):
 N[0]+=1
 if N[0]>=20000:return ev(b,t)
 if d<=0:
  st=ev(b,t)
  al=max(al,st)
  if st>=be:return st
  ms=[m for m in gen(b,t,k,e)if cv(b,m,e)]
  ms.sort(key=lambda m:-cv(b,m,e))
  for m in ms:
   n,nk,ne=apply(b,m,t,k,e)
   sc=-search(n,-t,nk,ne,d-1,-be,-al)
   al=max(al,sc)
   if al>=be:break
  return al
 ms=gen(b,t,k,e)
 if not ms:
  return(0,-10000-d)[att(b,b.index(6*t),-t)]
 ms.sort(key=lambda m:-(cv(b,m,e)+V[m[2]]))
 best=-10**5
 for m in ms:
  n,nk,ne=apply(b,m,t,k,e)
  sc=-search(n,-t,nk,ne,d-1,-be,-al)
  best=max(best,sc);al=max(al,best)
  if al>=be:break
 return best
def uci(m):return S(m[0])+S(m[1])+("  nbrq"[m[2]]if m[2]else"")
def choose(fen,legal):
 try:
  b,t,k,e=parse(fen);ms=gen(b,t,k,e);mp={uci(m):m for m in ms}
  N[0]=0;bs=-100001;bu=""
  for u in sorted(legal,key=lambda s:(-cv(b,mp[s],e),s)if s in mp else(0,s)):
   m=mp.get(u)
   if not m:continue
   n,nk,ne=apply(b,m,t,k,e)
   sc=-search(n,-t,nk,ne,3,-10**5,10**5)
   if sc>bs or sc==bs and u<bu:bs,bu=sc,u
  if bu:return bu
 except:pass
 return sorted(legal or[""])[0]
while True:
 g=post("/game",{})
 if g["status"]=="done":break
 while g["status"]=="your_move":
  mv=choose(g["fen"],g["legal_moves"]);n=g["game"]
  if not mv:g=post("/resign",{"game":n})
  else:g=post("/move",{"game":n,"move":mv})
