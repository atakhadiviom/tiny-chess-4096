import json,os,random,urllib.request
NODES=60000
MAXD=12
INF=32000
MATE=30000
VL=[100,100,320,330,500,900,0]
PH=[0,0,1,1,2,4,0]
SQ=[r*16+f for r in range(8) for f in range(8)]
ON=lambda q:not q&136
NT=[[q+d for d in(33,31,18,14,-33,-31,-18,-14)if ON(q+d)]for q in range(128)]
KT=[[q+d for d in(1,-1,16,-16,15,17,-15,-17)if ON(q+d)]for q in range(128)]
def ray(q,d):
 r=[];q+=d
 while ON(q):r.append(q);q+=d
 return r
RY=[[ray(q,d)for d in(1,-1,16,-16,15,17,-15,-17)]for q in range(128)]
CM=[15]*128
CM[0]=13;CM[7]=14;CM[4]=12;CM[112]=7;CM[119]=11;CM[116]=3
rg=random.Random(12345)
Z=[[rg.getrandbits(64)for _ in range(128)]for _ in range(13)]
ZS=rg.getrandbits(64)
ZC=[rg.getrandbits(64)for _ in range(16)]
ZE=[rg.getrandbits(64)for _ in range(128)]+[0]
T=["0 0 0 0 6 6 6 6 2 2 3 5 1 1 2 4 0 0 1 4 1 0 -1 1 1 2 2 -4 0 0 0 0",
"0 0 0 0 1 2 2 2 -1 0 0 0 -1 0 0 0 -1 0 0 0 -1 0 0 0 -1 0 0 0 0 0 0 1",
"-6 -8 -8 -10 -6 -8 -8 -10 -6 -8 -8 -10 -6 -8 -8 -10 -4 -6 -6 -8 -2 -4 -4 -4 4 4 0 0 4 6 2 0",
"-10 -8 -6 -4 -6 -4 -2 0 -6 -2 4 6 -6 -2 6 8 -6 -2 6 8 -6 -2 4 6 -6 -6 0 0 -10 -6 -6 -6"]
T=[[5*int(t.split()[i//8*4+min(i%8,7-i%8)])for i in range(64)]for t in T]
CD=[max(3-(q&7),(q&7)-4)+max(3-(q>>4),(q>>4)-4)for q in range(128)]
PM=[[0]*128 for _ in range(13)];KE=[0]*128
for p in range(1,7):
 for i in range(64):
  q=(7-i//8)*16+i%8;a=VL[p]+(T[0][i]if p==1 else T[1][i]if p==4 else T[2][i]if p==6 else(0,0,10,5,0,2)[p-1]*(3-CD[q]))
  PM[p][q]=a;PM[-p][q^112]=-a;KE[q]=T[3][i]-T[2][i]
PP=[0,10,20,35,60,100,150,0]
B=[0]*128;K=[0,4,116];U=[];HL=[];GS=set();EC={}
S=1;C=0;E=-1;H=0;HS=0;N=0
TT={};KL=[[0,0]for _ in range(128)];HI=[0]*16384;RM=[]
def sqn(s):return(int(s[1])-1)*16+ord(s[0])-97
def hsh():
 h=ZC[C]^(ZS if S<0 else 0)^ZE[E]
 for q in SQ:
  if B[q]:h^=Z[B[q]][q]
 return h
def load(fen):
 global S,C,E,H,HS
 f=fen.split();B[:]=[0]*128;r=7;c=0
 for ch in f[0]:
  if ch=='/':r-=1;c=0
  elif ch.isdigit():c+=int(ch)
  else:
   p='PNBRQK'.find(ch.upper())+1;q=r*16+c
   B[q]=p if ch.isupper()else-p
   if p==6:K[1 if ch.isupper()else-1]=q
   c+=1
 S=1 if f[1]=='w'else-1
 C=sum(b for ch,b in zip('KQkq',(1,2,4,8))if ch in f[2])
 E=sqn(f[3])if f[3]!='-'else-1
 H=int(f[4])if len(f)>4 else 0
 HS=hsh();U.clear();HL[:]=[HS]
def att(q,b):
 for d in(15,17):
  t=q-d*b
  if ON(t)and B[t]==b:return 1
 for t in NT[q]:
  if B[t]==2*b:return 1
 for t in KT[q]:
  if B[t]==6*b:return 1
 for i in range(8):
  s=4 if i<4 else 3
  for t in RY[q][i]:
   p=B[t]
   if p:
    if p==b*s or p==5*b:return 1
    break
 return 0
def gen(qo=0):
 s=S;M=[];a=M.append
 for f in SQ:
  p=B[f]*s
  if p<=0:continue
  if p==1:
   t=f+16*s;pr=t>>4 in(0,7);P=(5,)if qo else(5,4,3,2)
   if not B[t]:
    if pr:M+=[f|t<<7|k<<14 for k in P]
    elif not qo:
     a(f|t<<7)
     if f>>4==(1 if s>0 else 6)and not B[t+16*s]:a(f|(t+16*s)<<7)
   for t in(f+15*s,f+17*s):
    if ON(t)and(B[t]*s<0 or t==E):
     if pr:M+=[f|t<<7|k<<14 for k in P]
     else:a(f|t<<7)
  elif p==2 or p==6:
   for t in(NT if p==2 else KT)[f]:
    x=B[t]*s
    if x<0 or not x and not qo:a(f|t<<7)
  else:
   for r in RY[f][4 if p==3 else 0:4 if p==4 else 8]:
    for t in r:
     x=B[t]*s
     if x>0:break
     if x<0:a(f|t<<7);break
     if not qo:a(f|t<<7)
 if not qo:
  o=0 if s>0 else 112;c=C>>(0 if s>0 else 2)
  if c&1 and not B[o+5]|B[o+6]and not any(att(o+i,-s)for i in(4,5,6)):a(o+4|o+6<<7)
  if c&2 and not B[o+1]|B[o+2]|B[o+3]and not any(att(o+i,-s)for i in(4,3,2)):a(o+4|o+2<<7)
 return M
def mk(m):
 global S,C,E,H,HS
 f=m&127;t=m>>7&127;pr=m>>14;p=B[f];x=B[t];s=S
 U.append((x,C,E,H,HS,m))
 h=HS^Z[p][f]^ZC[C]^ZS^ZE[E]
 if x:h^=Z[x][t]
 B[t]=p;B[f]=0;a=abs(p);H=0 if a==1 or x else H+1;ne=-1
 if a==1:
  if t==E:c=t-16*s;h^=Z[-s][c];B[c]=0
  elif abs(t-f)==32:ne=f+t>>1
  if pr:p=pr*s;B[t]=p
 elif a==6:
  K[s]=t
  if abs(t-f)==2:
   rf,rt=(f+3,f+1)if t>f else(f-4,f-1);B[rt]=B[rf];B[rf]=0;h^=Z[4*s][rf]^Z[4*s][rt]
 h^=Z[p][t];C&=CM[f]&CM[t];E=ne;h^=ZC[C]^ZE[E]
 S=-s;HS=h;HL.append(h)
def um():
 global S,C,E,H,HS
 x,C,E,H,HS,m=U.pop();HL.pop();S=s=-S
 f=m&127;t=m>>7&127;p=s if m>>14 else B[t]
 B[f]=p;B[t]=x;a=abs(p)
 if a==1 and t==E:B[t-16*s]=-s
 elif a==6:
  K[s]=f
  if abs(t-f)==2:
   rf,rt=(f+3,f+1)if t>f else(f-4,f-1);B[rf]=B[rt];B[rt]=0
def mkn():
 global S,E,H,HS
 U.append((0,C,E,H,HS,0));HS^=ZS^ZE[E];E=-1;H=0;S=-S;HL.append(HS)
def umn():
 global S,C,E,H,HS
 _,C,E,H,HS,_=U.pop();HL.pop();S=-S
def ev():
 v=EC.get(HS)
 if v is not None:return v
 mg=ph=mt=st=pa=ks=0;P=[0,[0]*10,[0]*10];F=[0,[-1]*10,[-1]*10];bi=[0,0,0];Q=[0,0,0];rk=[];pw=[]
 for q in SQ:
  p=B[q]
  if p:
   mg+=PM[p][q];c=1 if p>0 else-1;a=p*c;ph+=PH[a];f=(q&7)+1;r=q>>4;mt+=VL[a]*c
   if c<0:r=7-r
   if a==1:
    pw.append((f,r,c));P[c][f]+=1
    if 7-r>F[c][f]:F[c][f]=7-r
   elif a<5:
    n=0
    if a==2:
     for t in NT[q]:
      if B[t]*c<=0:n+=1
     st+=c*(4*n-16)
    else:
     for l in RY[q][4 if a==3 else 0:8 if a==3 else 4]:
      for t in l:
       x=B[t]*c
       if x>0:break
       n+=1
       if x:break
     if a==3:bi[c]+=1;st+=c*(5*n-30)
     else:rk.append((f,r,c));st+=c*(3*n-21)
   elif a==5:Q[c]=q
 for c in(1,-1):
  if bi[c]>1:st+=30*c
  for f in range(1,9):
   if P[c][f]>1:st-=12*c*(P[c][f]-1)
 for f,r,c in pw:
  p=P[c];o=F[-c]
  if not p[f-1]|p[f+1]:st-=12*c
  if o[f-1]<=r and o[f]<=r and o[f+1]<=r:pa+=c*PP[r]
 for f,r,c in rk:
  if not P[c][f]:st+=c*(8 if P[-c][f]else 15)
  k=K[c];g=k&7
  if r==0 and k>>4==(0 if c>0 else 7)and(g>4 and f>g+1 or g<3 and f<=g):ks-=c*25
 for c in(1,-1):
  k=K[c];g=k&7;y=0
  if(k>>4)==(0 if c>0 else 7):y+=8*sum(B[t]==c for t in(k+16*c-1,k+16*c,k+16*c+1)if ON(t))
  for f in(g,g+1,g+2):
   if not P[c][f]:y-=12
  q=Q[-c]
  if q:y+=2*(abs((q&7)-g)+abs((q>>4)-(k>>4)))
  ks+=c*y
 ph=min(ph,24);eg=mg+st+pa+KE[K[1]]-KE[K[-1]^112];mg+=st+pa//2+ks
 if ph<=10 and abs(mt)>=300:
  w=1 if mt>0 else-1;l=K[-w];g=K[w]
  eg+=w*(10*CD[l]+4*(14-abs((l&7)-(g&7))-abs((l>>4)-(g>>4))))
 v=EC[HS]=S*(mg*ph+eg*(24-ph))//24+8
 return v
def sk(m,tm,ply):
 if m==tm:return-9**9
 x=B[m>>7&127]
 if x:return-(10**6+10*VL[abs(x)]-abs(B[m&127]))
 if m>>14:return-(9*10**5+(m>>14))
 k=KL[ply]
 if m==k[0]:return-8*10**5
 if m==k[1]:return-79*10**4
 return-min(HI[m&16383],7*10**5)
def tick():
 global N
 N+=1
 if N>NODES:raise TimeoutError
def qs(a,b,ply):
 tick();sp=ev()
 if sp>=b:return sp
 if sp>a:a=sp
 ms=gen(1);ms.sort(key=lambda m:sk(m,0,ply))
 for m in ms:
  if not m>>14 and sp+VL[abs(B[m>>7&127])]+200<=a:continue
  mk(m)
  if att(K[-S],S):um();continue
  sc=-qs(-b,-a,ply+1);um()
  if sc>a:
   a=sc
   if a>=b:return a
 return a
def ab(d,a,b,ply,nl):
 n=len(HL)-1
 if ply and(H>=100 or HS in GS or HL.index(HS,max(0,n-H))<n):return 0
 ch=att(K[S],-S)
 if ch:d+=1
 if d<=0:return qs(a,b,ply)
 tick()
 e=TT.get(HS);tm=0
 if e:
  tm=e[3];s=e[2]
  if s>MATE-200:s-=ply
  elif s<200-MATE:s+=ply
  if ply and e[0]>=d and(e[1]==0 or e[1]==1 and s>=b or e[1]==2 and s<=a):return s
 fu=0
 if b-a==1 and not ch and abs(b)<MATE-200:
  se=ev()
  if d<3:
   if se-120*d>=b:return se
   fu=se+150*d<=a
  if nl and d>=3 and any(1<B[q]*S<6 for q in SQ):
   mkn();sc=-ab(d-3,-b,-b+1,ply+1,0);umn()
   if sc>=b:return b
 ms=RM[:]if not ply else gen();ms.sort(key=lambda m:sk(m,tm,ply))
 bs=-INF;bm=0;a0=a;n=0
 for m in ms:
  qt=not B[m>>7&127]and not m>>14
  mk(m)
  if att(K[-S],S):um();continue
  n+=1;gc=att(K[S],-S)
  if fu and n>1 and qt and not gc:um();continue
  if n==1:sc=-ab(d-1,-b,-a,ply+1,1)
  else:
   r=(1 if d<5 else 2)if d>=3 and n>4 and qt and not ch and not gc else 0
   sc=-ab(d-1-r,-a-1,-a,ply+1,1)
   if sc>a and(r or sc<b):sc=-ab(d-1,-b,-a,ply+1,1)
  um()
  if sc>bs:
   bs=sc;bm=m
   if sc>a:
    a=sc
    if a>=b:
     if qt:
      k=KL[ply]
      if k[0]!=m:k[1]=k[0];k[0]=m
      HI[m&16383]+=d*d
     break
 if not n:return-MATE+ply if ch else 0
 if len(TT)>200000:TT.clear()
 s=bs
 if s>MATE-200:s+=ply
 elif s<200-MATE:s-=ply
 TT[HS]=(d,1 if bs>=b else 2 if bs<=a0 else 0,s,bm)
 return bs
def mi(u):return sqn(u)|sqn(u[2:])<<7|('  nbrq'.find(u[4])if len(u)>4 else 0)<<14
def think(fen,legal,gh=()):
 global N
 load(fen);GS.clear()
 if H:GS.update(gh[-H:])
 D={mi(u):u for u in legal};RM[:]=D
 if len(EC)>300000:EC.clear()
 HI[:]=[h>>1 for h in HI]
 N=0;best=RM[0];sc=0;h0=HS
 try:
  for d in range(1,MAXD+1):
   w=40 if d>1 else INF;a=sc-w;b=sc+w
   while 1:
    sc=ab(d,a,b,0,0)
    if sc<=a:a=-INF
    elif sc>=b:b=INF
    else:break
   best=TT[h0][3]
 except TimeoutError:pass
 return D.get(best,legal[0])
URL=os.environ.get('MAKEFUN_ENV_URL')
def post(p,b):
 r=urllib.request.Request(URL+p,json.dumps(b).encode(),{'content-type':'application/json'})
 with urllib.request.urlopen(r)as f:return json.load(f)
if URL:
 while 1:
  g=post('/game',{})
  if g['status']=='done':break
  while g['status']=='your_move':
   load(g['start_fen']);gh=[]
   for u in g['moves']:gh.append(HS);mk(mi(u))
   L=g['legal_moves'];mv=think(g['fen'],L,gh)
   for m in[mv]+L:
    try:g=post('/move',{'game':g['game'],'move':m});break
    except urllib.request.HTTPError as e:
     if e.code!=400:raise
