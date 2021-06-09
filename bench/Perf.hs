{-# OPTIONS_GHC -freduction-depth=0 #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Main where

import Html

import qualified Blaze             as BL
import qualified ExampleBlaze      as BL
import qualified Small             as S
import qualified Medium            as M
import qualified ExampleTypeOfHtml as ET

import Data.Word
import Criterion.Main
import Data.String
import System.Random
import System.IO.Unsafe
import Data.Proxy
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Text.Lazy   as LT
import qualified Data.Text        as T
import Data.ByteString.Lazy (foldl')

main :: IO ()
main = defaultMain
  [ small
  , string
  , comparison
  , scaling
  ]

small :: Benchmark
small = bgroup "Small"
  [ bench "oneElement"                   $ nf (run . S.oneElement) ""
  , bench "nestedElement"                $ nf (run . S.nestedElement) ""
  , bench "parallelElement"              $ nf (run . S.parallelElement) ""
  , bench "listElement"                  $ nf (run . S.listElement) ""
  , bench "Int"                          $ nf run (123456789 :: Int)
  , bench "Integer"                      $ nf run (123456789 :: Integer)
  , bench "Double"                       $ nf run (123456789 :: Double)
  , bench "Float"                        $ nf run (123456789 :: Float)
  , bench "Word"                         $ nf run (123456789 :: Word)
  , bench "Proxy"                        $ nf run (Proxy :: Proxy "abc")
  , bench "Char"                         $ nf run 'a'
  , bench "oneElement Proxy"             $ nf (run . S.oneElement) (Proxy :: Proxy "abc")
  , bench "()"                           $ nf run ()
  , bench "oneElement ()"                $ nf (run . S.oneElement) ()
  , bench "oneAttribute"                 $ nf (run . ClassA) ""
  , bench "oneAttribute ()"              $ nf (run . ClassA)  ()
  , bench "oneAttribute Proxy"           $ nf (run . ClassA)  (Proxy :: Proxy "abc")
  , bench "parallelAttribute"            $ nf (\x -> run $ ClassA x # IdA x) ""
  , bench "elementWithAttribute"         $ nf (\x -> run $ Div :@ (ClassA x) :> x) ""
  , bench "elementWithParallelAttribute" $ nf (\x -> run $ Div :@ (ClassA x # IdA x) :> x) ""
  , bench "listOfAttributes"             $ nf (\x -> run [ClassA x, ClassA x]) ""
  , bench "listOfListOf"                 $ nf (\x -> run $ Div :> [I :> [Span :> x]]) ""
  ]

string :: Benchmark
string = bgroup "String"
  [ bench "randomString"        $ nf (run . (# randomString)) (1::Int)
  , bench "randomStringRaw"     $ nf (run . (# randomStringRaw)) (1::Int)
  , bench "randomTextStrict"    $ nf (run . (# randomTextStrict)) (1::Int)
  , bench "randomTextStrictRaw" $ nf (run . (# randomTextStrictRaw)) (1::Int)
  , bench "randomTextLazy"      $ nf (run . (# randomTextLazy)) (1::Int)
  , bench "randomTextLazyRaw"   $ nf (run . (# randomTextLazyRaw)) (1::Int)
  , bench "literalString"       $ nf (run . (# "ASCII: aeiou {v{H5@!(c@i/e^93q7+.8?u)c:F?&_*jQ:{s-G[MIO*!.F$%dXDROgf3{Sts6zR)RT\\RM#5^~3/uKGCb2i,\\EXMN;hb0CJ/?9s4KHet-Fr-kV9H~T<RLpzQ9IV3OJexSv*QFk !%Ebyg4[$0{~-{{SS7nn(!D@LbOi-)Etz'z@H+s]VJ!'a>_:UJ7^zxZt=1*JdEF%U wPzEb!$GN7v.b?k99^(il6*a!Z/$bbHhv.8sor lm1j\\i\\;BBlD|sOSwm,f9hrc3rtpR454Lmqi0!'Xfk>L>;*%a(.W+tT)4f/yOHR_.Vscl3DIkeNs8s84B4WfzL&1^oiL6|Iv{4)v7&j_Y2)KYGp_\"`e#B{.TYqFyrXinq1*/=JTquF,*_pY`tdoi2~(?bZqS?i$5fSVmIE6/FW/F=^`5=.=v#\"]+{lw/(VUvY$'kOld,M'rKQY\\fO}+G|n-!X\\5@8L0C|hUMj5/8Mq`Rzg#j;VCmd}[+*wPb>{$?1e|0xZ\"'u?YWx7\"]>,gK^GgJ_F5ea1@e'{ K37nSU[h8_[K<kvAKA2+_&O}Jh-pxQqxS{78ZS9EBp_54vnjs@~>raHm)R> @Rht{je~,^7'9m8*O;2}x'{Z/B5l$~+jV.br@tFrFf?C1,[1fkjPzK?f.GP4 y-d)=#n'+99B0$V^^flg`AA9YlQ4t<gZ(8]JKyJz9OE$&oTY*?[N!Xf?d.Xm5\\-lC/!h$dqmh%{6\")B=oh/$4vQp3mvu`}rg] lwtyF5@nik&Tqn{}I_@$E!fa*8PmOGU6i06s,}NbW!8bH+gm1Zd,~du@>:Om.',9D2:c%i~YXNyJ2cQg=SVb34~c).1@U7'?_*JMkZWMpgO)&P(_QMAyolbwb%+Ep!?j;tcjEat>g>u97tfQ bNU#A783O1=K@)C;BW 4XlHor4}alsfCW[!3<=;(9fIeBE=AHd,`k!<V3-`fyP`3[4\"2 d,o(b<dZ4uMPY'bW8DT<T0&jFFg5dJ?hs23NLqBf+0RAfrj&RW3t]FI[xQh8L^6fYS&w`'N\"iSYTG%MR\"g9Li6]GK.<yiRX)NZXvbj\\-!if=@{J=\\T;GsE#(*/qy3v-7b]EYNScTsO:wH9vq0aybQuF+}H&],pp(S8tmh;\"^Q*&+ea`[45Zz'Wfn4-Ot=&;LmHfcta^b`@2xxg^B~k./o:ox-!(O:hmynqMhrJN~Fl82>q94D8d^|v^B?(O;G*b|Zt_#!pdM[qD8Ki>fnNzRGY*;1oI{><!``Lj8Y@\\%G%|!v KU)!.7U@kM!kdE>?Kad4b6XUq^MmF=r+P\\VuP78*-stkdnA%;ogs\"2d&#+fu8F9BXE(h%o,aoy$oufM\"(CGaO+lZqEuQ{`LL#b}Cog|y^N{WL*'1`KXHBS&13#ZEdvHE?:${N/wM ^}m40qaX6Qt_+aOu~6L,La rHkaEu\\%tDYLGHj3\\Yso7o\\rx%,_F^znD*}*mv9I$$\"}jg2ZP#f ,;*9|\\xi^{|;2)KtbM=U?Zr_:gI$OoXngAQ4ec.>d7>~1.W{OI.Rn*cMO4 3 CLR>h%c:C`\"J{0tX,V9f2!b;nIjP3;!(\\?G.dV`uI8!\">N;yoN9@;IVFg@w{l.h7K{zbQn^,=v[_%y[IdO3If#gGZ/WtDsAus!3>RymX>D-G7^hI~wj!'@w`CFN88Z_2p1NKC@iW-UB8S9kk.EWgTrWJe=s{g`:[{kn<J,I_C}ti.y5]0|qnfGjTCLK moKExUlh]kiM\">.bRIn^gM1y,u#RA|VOO,Ay39<voplYY:AR'_ovZ%6~@aI<AJt1Ms+/qd)]Mo9Te 8$k>tB&YkxopGc*tQT`y*\"rv\"!3l<lY.=j/%P>\"rsHfYkaMBZ+%HJ-'sl( |Puq\\dM{H% Wr:\">b1<|}7c#+Q]L,>*WU2(%~SBGiS6u[yB2iaYmV23b&c0BFbn\"c1/Gf;`1\\uKGw\\,tj#fsx+73*am~^>f/GwQ75-\"/]C*|e`ys}]]XfdZcg0>AseXE:*B5&.)^9@:$j0f;eeN6,j7{Q|K'6O<L6+}?eE,Dziy5I#b4xUKn0P[1(xkxG_Nf6(Qv$.B\";iDT_WYzcx1zrkSQAcAjG9Z:4(.|`uH5@r$8 h&gf wr!}.f\\/I4h-&~-lm`cBhaD8z9d!Sn|l}!wM].;5Lb<)<gNGcc-J.)3l=oJtQ/,>n-#\"Rav0a=LTUX9mIkH?Y$^a}U|LwOGa}3w3q7iB.cs}:up/*)#Fny34()\"ebt!]J7K,nZ8tOC,48\\gs;P]H+qWHWm\\LCy'6$pNLM}^haz&H(z[[EIXrBxHYD`5i17 )>$oDK\\xe:OEny6B}F`%7CR|k4G\\I?r~s(o:?\"(}s4{VT9+id@N:bzFC E[&%N}2G+VMbv}XC[BZ[kJ]V=gL-Ph](TVe,vpzaK7B0+K|c<:n0mSm3.:2U,j4cwZP1U1d{:5SWf(f[NXXM~7-_bk%oK{O@I|5U)o<zjE#Yy4~\\>n,\"-mC!uLF*`Pw\\vIOS7XJ.h~]t^^9CY#3H-1]g6s,_g_RyU-9TqccG3V)aa_}0IinfD6|?2+vQyRj_Q<s\\nh33^P+\"{RPOBr.?R4?(=dFKHP4bc6yfrgRQs*NyY(v;&UAgN`Z!yq\"4R940ksl[-WA]nk|4TPz1N|5bFhWC-q!+D.='B]d$3|w^)[_dB%((W!t=ToZ{0\\GP'&R^s*~0`28r)7Mkx@NfJ#cUGS9J|,rPFTj> mWIAJBzw(/,OSB?xD&b}S}?? yX1!PDYWd.%!k@(:=hJZ%{.|;-ENBE>t$Z!,cr3J6:Rs;;.[}(OA$C(dh;?\"iC- \\ J?:+RBEdHQ`nj>.sS#4zXMwbSQRlNXAjz9C#:w~Yk&iYh)ZZz{RC99~6HWJFl:lFLxbgb=z,+l$P \\|Ak8UGx]64h.V_ Eg*cR9)D`G7{r5~XJ:lk0K'#~bIam9w'L'-l(xP`tz:9*3f/ROz9EM=MW2Yd~q!\\'47]3p]KR@.nniZ8l,{5HU>EUQxRX(V n:EcL_h<u<YU-D5C~c?<>Lj#3V;yl*2czy~b ch}_[2,|j9D|HJS%em-=;'h;hqF$eDH]h<fh|s$_!r5{HRQu#g{q;xb?Q3s?!4:+K(EEds:p(idYI+a<.1'{7M!`$B],i-3p;w}~8lWkFL#\"~MVP/_R?yIoTarf 9vqgvH%$ArC)e'.(DAcbwG/'W|BQIO}|?Dc^QE%ukdj~[z]okuGXzP!IBT>DbtxgRjM=Je08AV9y-~gZ $KN_o=&@$qwE]^?D_'I5t3\"yS:_$\"'sC 9!Wvzh?58Ax5h `DQO/\\bG`t2!7_Fj#%qC%5+cA9-m9Xu*$`M4gJ\",5#*1:i=|]{i'pf:[hOTH7l#H~lKQfDjK/NyFV:ZM+HJ{W\\[4S!*lFP5-@sMsJW%,`'!J4[Jcs0WG/&|0)CBugj4fr,$-.<$NV,aGX4 @XrKga!`t@H.9x1PQ:9e^m8e3CiTT/wR^vEB+]?yGw.so(%>ZaGNn|@`ffeI>HoPBO&2L'3FMyI\"'BdObllKr8]D\\t(S&>`VXo2G.H#E`T'M3GQ3+6f:h^Na^5tBA1BV<]$]-K;ENy)\"7Y-4a|K57oeTE^b%.zkG[87vD 6#^[8*%SrSbg%;5*M{?s|vV{8\\)TPKZq']-j^wMQsu.-]_[r*<+hzZ@v:M::z3CqM]prE!8!e7D~-N>&#6,FLb~ lA3GAC:\"oE}?m4hq'nfJ.=BQL#dyl/Qhw2bmgWt=JX_]FM3rSgJ2Hg`3?41R%\"\"iN[MD;O4-xX8B3L,F+9g! iEZ[3(97qQ/(pGZzY:BY8$ps#E`j}Trl~lyIrj{`'VDraHaMN-Y]%V\\U:tQ*m1W)V}|''-8wIBQ(01u|5_,.ZE3g@m9\";q_AbOa7TvelHS/|(tfJBm1KBBct+c}<#S}`?p>0)b^p)]4E\\*j3I`?L_~Oqhmh#*:,meS)Z<{~'~1],51n itIBtO+enEm}nH35ckS?^*hzv\"(~`AA|i\\emYgM!N40Ll@=)8AL[j<FM7+o|:e)**;w]XEE_3#,:on%_K,!TQ25ucrWoz_[Hs>:)^wkN?Qw+Fw,{aGORlUn`%g%)\",PW.y@#w`\"H,e#D^9WeGOP3: r:X(Tq35[as%\\r6{lC1$cx9NX\"FhjS -1ZksJrkoBFZbK4P^@~wm`RZ/@&i?~lv[1=A[Y!VlcfC(m=jL\"/=.aTkv{ W=s:xEcKO/^LA@o'[`C^*Iga/<'Rs\\:O!-&,xpZ-tPZY0H'rb*Ja\\.Ok2jl)$::\\|p#|9;goJ<'^7E^t(W~S@dKIg_KwlP:\" 7l|s$-hP^Hit<s~e6GGCF3:L<+-fEKmqS5eQ=,.Y\"@&PYtP>~Dzr,'zh.2A& 8,<Jx*2Zg(Ox_Myg[?0\"6V<,U~:(\"Rr{/^*&AFs2UMyL:gE(V,.8.@U*e8V5&E6fcfZL%=&b^3fE8\\+#r<fC'&0E)9'A`}3!O{TONVI:BP{3;3W:9z6b<4g=42jhPS>]}Xb!p#GS[^#F}gFPxm|5pfjj=t?FYU37#ABC4`S@[UU0*r|_F`!1;QL@K,>G53t_S\"Y{f;#4*&wnJ%V-$+P7Az}k8Fqw=cxH!R^h]w:Ydgn<12h$Hvx!qV#&`{*@\\5yFdi\\HYX$*ij2R5MU_ExkuqG1}}T&66zrFC@R#%]ml{\"j&^&V[kq56A3-.4>G6( @7-\"7Q3lDm),lvL2P|b5$$+#L&fcWfwNv9Wwn4}S'j%urMWceqOmZ&sO)Id}wY&Ak#t-ut^Ci^wnR5eS6ez4{P|k/TjrdK@|7KF$9~Q)Eso@C2m(*&2sf[J4 Pl`uzm3CfZ=XRP0L&FhPDC#$aP`~teiQ6A8p/mOIBN{2J!0e4qrLSUpvRl<2-AK($Lwa%e2)xBhZ9Lt[fL%7\"Vp~$[WA=,?:2Jb3h$uwvPA4V&b{tTVS rhdI\\c][:ba&o\\;2YSNg gz,8@/.Kvsjp+OI6zgInKS~vh,*F]v3v.4~:m;i#5E8aYc]J)B:`K.N[;Zo>hGoF>S29G4/+oQ/i>OXxn^%i$O}|SI\\!'z@wSKCe2]Fj$[L\\YDDvs{}|%i uou6i]\\n5=Y+==FJIpjBi42[|*x(IZ.Qt8(w^qD+&<f^Wkk:f7:_G$a:<B,]*W3SsoD&)ze&h;rf<Iy6&ECGVN'?0Nskh q HG=|}zNEshQtp6b41HP+]M((62(f1eB\\Ug)Qs-K].\"`g}Lj#h|4D`L*q$*OcD3)(HD60(f3h?ie=t]7(L8@^];H[To%n|uT''Xkf~Ls{y<0Z\\O(rq9Vgr_%,O*GL}eDb:Q/zB$6%RX#1_uLo:o#F)G@H|*Zu!{B..zC$7qSwhR!&OeR[4o&V88VB]_QZp0:8sBsz+12OQrr`wqKv6GM>1RS#/E998/gc]L.{2/$#$D+:J4x.}WEK3l c|-=>g`q*Y%P..8j%XYpl&n`8,^/?d$m`*0$\\BY_u#pW7f\"9;bvibvZYd$dT++K1tI*TmF{7B=b&N\"{SV:^U2\\h\"7+Ky_p&,2K!hk;61]X<l-o4$=,NuI*G!T_OXI((('QIW:}7uGoU6,U!{QQPi[9z$aKv~.=ZS9m4-NJrReG6h1*^'E?]v83=3$cS(>C!BFb4f(\\XkOCcHSJ<)It5dI+2r~8-eLY20&<\"~`v,uPq3\"Z8$1^B{%R*o$X4$$G^j\"I/*O.&`n)j,53#axtjx\\S%+12,##~L7[|wH?^%uUR}Ow&U/.&iy)#I|KD:\"K[!z:d1$;>~_5,[b?s?$^qen;O|4vf/P<:aShNx#S:^VS5sikFU>~ BObjd+\\j`(@0SQs`,a-*E`!GtxOe'n 0t[OL>'s`3^~[fCUY<_|IaEasw:wD5FV9R'0X43Y}]RDjl,&aq#6i|s_`g=PM^V9yk$@Fvy?MN\\9wdtYt#.kqR2G8i?ND;LqMU?d'L3~]#x{KT\\rNkl7;_ceL}{B-~6$jt}~ZTP/RwwI\\`l6dVN{D1 tkc|8!ATe$@ohoX)z*<ZK~]S2R`F&:0hUn3L<P8l0 M?j\"UubIn.1=b$eogi~I,ydIh4wnw|5y,bQD?Y(:Cx2a?bfi<2h'd*$@!/{A,VFM&]b''j(7r.tc%`F+|LN4B2dUx/)of[HP.(hV%K-g&;nwwG 3'RRAEK\"81[F5@JCd{D`T~&YeS-m8m?j0n)Cb@*HO_&)i<O+U=QQ0<}T|3QO&ReY/gxkto8Jzf%w^'l/4,z,Vykd@ I@IS{ tH7t,7p(OJpZ-DPPFM{5[ngaFk/^~'IE_mlR%ayA*?f HjD5/.XD&+rr/lK*1m;^ObyB+hjl d$23m*i\\6Pu[:4ZG\"- ~8GrGRd>$nM]U0tiahQo7^`pR},\\9.2Vt00Op.hhL+o}@h{aKg+1LDhyG5$n[0sxwFzH;%{&^5O`Z)LozW.Lu<n/rz$1@9| ~t*?L~HHx9:]<BAEV]u/Kb^/=&H?nfUyCa1y&A7xaAJ%E(C=xEB%l<'4GMbq`i_!'WUR~++ ln/u'h9 kzw\"Q's?z7*{p2=iHz\\JoK^jA=TGVe~NY3nl[T)'0`2W^7hEnds8N$$g?qgig]VV`nI#)g!Ck?7\"S3\"-HjHre:\"2cX}pH1u@tk^c=Bm`?6#cOhbD/&R[Z*X)$hal+M@\"$ hj;GN;e8),CQV0 `=itZYz\\2oY5EFE@T.~4L}uoo%%:|;/4A59p0&W--zA}~@*4<6af\\6fY7 h8^jeYc\\em=C4'[dl.eMcy;eiAmD]GH\\I~h&R`?<%L)I+%R@\\eyvQu&0zyh3&M_7`&o%)Y^7TqN!BiHTR*pT`&{RR@Nub4yd=FG\\a5=e$HNO_(WB#?@kak S'ubUEw1vL~!VaMxa~r?sG)\\A Y3W.s*=/Mf.R'8:.&cTX=('qzE-{?->Vy/l`Dt}Cfz,+[vu\\~z.\"Z!8tqKmW4eRZfFiyw[@'X?Um(w A:iR?bL]eK%lp=|{K0]qdn,B:t)e]yEDwn!/Xsw:6w0hwK2iH|e&sy$61eja8cWQ~?UN..p|xA3Jm`~j9(w#&#Qv5*5Cbt$N2hF!]EKLE~iePo$,I5hf,/n<cL0\"gP!J\\sRL~+ 3[?Zl,AGd0ZOCDL0O_+vLcAJS^$ IxpR>v-8e{\\Rhza],.AtQY@oB,,E5.VIWh#@k`0v6}WB~A|6tLzG\\A.V:cOcGy-d2XBj4OUN;?7}mj{Cl*]bs8+#PKbU$?+qp=jcguw%BGC`}'orA:*&Cxnaf'3AI{7cLzE@SCrBYskD!2wDENR13zb>xQO/:k+[i<Xt}<Yunq{z3(Qk!\\a]J\"QB5m;]31nojJ4XT|z{|(&a*H}Qw0\"9rMv+#<TvXf{\"5fk+!rRAL9B)`s#3$FY&& m|o4AT;ROjnjzq\"Y/y<9\\nc3A,_Y\"40p\\{K[MY==g`SjhNv8b<C 6`WvY\\ssYd_2Q] a9D`35gH*2\\2/|JxC4&kLp\\~G97V)G\\C+i-mzz\\vW}xv~xYnad%q(Q?H0\\b?9xUz-'%yC>(MX%4t]\"~j0E$+=AD(o:e\" nj30,aD#7Y#M b JImD~&?vkq5mL@J1s]].y=FNjC-m$L9K/Jp6Y[Y2uU((\\8K$2]8X-[(m? (g!X3iQPv7Qj&+:D=[ ?WZ}X)<R/ttr Wu-Pe+5fRwRA@)Q@S:b}WAi}=d=yqfuNiP%o36f1Wjas5+BE0;50_K37czEEodwS2Y>+L-M&%>I'k>k88FBn^kJ\\`7}3W5w~(zI(JXrO=lO#j%/2<jrO=x!|TN|qH!:NElrXM5C(/#7,q!AIlru[<*qgOs>4(TN\"]rMrMUGo%r?diYRhytsvp#Ec^,Yu-tN3c`$IEU^LI ,\"ca}I3/~E)LZkq</E0hFBLUTg9$6uG8^QT3#wO+w }:MP[W9cpF%.IF*Kv~Bp0)i&u&j4Z`~FN*pwWI=75_-$b]B:X`XPvjCXrP_qK*'-vXg,w a1)qm4u-])D@IuJV7:yryo'ql+uMI{3<3IW3CH\\UGJnsW;5]U8ZZ<9e>8,Pf7WGdYk/]K:WqU`8pk)%iL:=q[MFE)\\roWx<<=(JQB(NP=,%1P4ghND:bvk0Q>TL_%sm_ZN';W1&%(>-X*e#UT/^;,A$dsYP&XD6'^$hd>x-,o:M9.WfnA/js,*z2-80$N* $O*$Nm@Wky6H{VG'wVa[IP(]K5pfbxsy.w83Wg^~>_Ynr+:JEUU^rMm xy0nL,bgc|Qsj\\\\&[a{kY@'{Fzp'BM\\,#3Lm^pT&@ad].},efGo'jHH\\zYFM~&#+/5bH={1zjemX5wsJ$R|JiED l^(]r]4?_{H%e[D`~6^Dy^BnCDn>?WuF&RbsLVc{/E-ulm,CL+(i*AnlM4UQ%-L![j[y))uE!=$2hc[/25L8rWCGh/{,#3Ot[X:eOv\"JP'\\+m3h!&{7BJ>U^x09&g};]@j,1i2y>j&ly6+NB~%0JtC#)$^-?jV$ #gJjo(*[J_$=M1O`*~M`^jmgHi-iHw5BJ}m5f=]Ilc/WaVAO\"<]}%P!,0*N?8\\R[ZFkuL`A;%YsCNs^3/y48k65_@(iB|nW2)84f)+=up,3l(yvp|)GYli_EI*+j}=r`!M}[4v6uEG9|Q \\uB.U^@_<G+qjn\"z(gmd7L?=*TT;^=& O_kBGV=&8(udEpd-^D`]xXM?_I6I01+{a$bt--EAe5_mGo|rk/}+)I\"k\\7uGDYN*xb{!\\P.}q)NQ}!J\"_^$xnDy-RDtfoq8o}lT:=1di k%&OD1lj0Ig;uQ0rKM!on#QXur,(V>?C;-hi/l,]v<zxxI[SHZd S!+LL3]7IhxoL,u.|wgocv=8b8 %`?3^0MU9)5>-w{d^e*,}1k\\'%b9}6zv0k*ct2P]im:pq^q<5]6g)OU)@`JmA<2ot70=+p+0L4+Yfxs2`6Y$kTZgr?af2xq|0r4z6U'(MZY61\"|%veDF&M36UWv!,jx?4$jp,f%V VTO*0wy+33s-41&<!y5,.!\"5C*,}^3qNk:Q7&SXNBfzAEZbW\"m>8wzMvX(/OD^2W^sNfmlM{IRZk0F@.>3iNg_aXK#d[.`N|83@B'b4*N`k=[5e{ZF\"G`Mt#?N\\kmI14DNI \\b4dO>(.B]\\S*bM4}qS?.BjF^Mrm:VE4A-N^c'w)YvMs/;!ccv\"MC\\lfMQu`Yf*a[8*@9A}a/HuE J`Es{{N  22euv&P'$=`)x\\,vaV!a8*RR.^q/R_x> r:_z>j-y&;'r\\U&T?D(xKns%7MIE~6I$3r)u!+OWwmNRJy)bk^S.1>2UxyBK #VG&hX69/Do+Xx'.RHp1RH$i}_I%|]o;iGMuS'^DZgZ\\a.N9a?(U G_@)voP5\\y<*kyxE5o?-G>p,Gbe6n 9-n~)y\"zTF)@kR'|GRb7mj_m#%p-kEewK3DZIqJ(|&c2R{\"T=t'&B<ur&mzlF &?VV-#. b5^/*Ve9kVbN(w-]%jM~ Y|Xh?4?9jVq9S!Q;f-~J%MH>3!Nci!6`mTH^'pXxyLH.VnVeJE}4n3/\"xEc/K?m1wbCpR 1SIeMpA>m`sVSfJBjqH<[ZOB;z6~t\"DH}nY1N*;-FS$&~?Hu_:ayFm]XHKV,H\"<^R&=Vh;LE)!0/G.Up'/Oz+jNwiRo\"c&x']mi\\D\"OC3KF)wKz;|3POvt%9!q9&c!1?bnnhm?8#Z)JR$d'r]?'!mV0n8Yk,o8U'G%UR>W7@aPn-f|9UXL5tPe}3NSs!C.jC0Y!u]O5@@;w).Kt8n%5L ';,}i}9>)_{ K<0T~P+m7|Lp%!v]4jIi]!7[bZsg=&Q~7Qb/{0# MMn-acT!L$$Ra+q#p9uI0blW{pE^XRYZ.V}j9v:4b|@9JJe|~<&vurWNDt-|moga{TzzZQiD|1\"b{TaY[|x((305cwRE\\`k9I\\}%P}Zs\\|D bhdcN'Gi}*7Zg_,v%Q]Q*!?DbG0cK^G(e|&.iTmsrFvT#_ek(.!;k]vqJ$\\242& I8Z~FE5~k3Hwz8-Mj(B,YUcHcB6)U;HMTvA)aq 80WyT_n8/N/NQnv+9#j\\Ta\\*FlSk3{suX^SZ*vn h1u5]ubT>MtI`f5kH,&Q9eWD\\6\"p. kc;j|yjm'p81E*:|d)>W5;URa.~yWg+\"%)TZ|>)cv\"|*]IVg|fFstUh")) (1::Int)
  , bench "literalStringUtf8"   $ nf (run . (# "UTF8:  äeiöü {v{H5@!(c@i/e^93q7+.8?u)c:F?&_*jQ:{s-G[MIO*!.F$%dXDROgf3{Sts6zR)RT\\RM#5^~3/uKGCb2i,\\EXMN;hb0CJ/?9s4KHet-Fr-kV9H~T<RLpzQ9IV3OJexSv*QFk !%Ebyg4[$0{~-{{SS7nn(!D@LbOi-)Etz'z@H+s]VJ!'a>_:UJ7^zxZt=1*JdEF%U wPzEb!$GN7v.b?k99^(il6*a!Z/$bbHhv.8sor lm1j\\i\\;BBlD|sOSwm,f9hrc3rtpR454Lmqi0!'Xfk>L>;*%a(.W+tT)4f/yOHR_.Vscl3DIkeNs8s84B4WfzL&1^oiL6|Iv{4)v7&j_Y2)KYGp_\"`e#B{.TYqFyrXinq1*/=JTquF,*_pY`tdoi2~(?bZqS?i$5fSVmIE6/FW/F=^`5=.=v#\"]+{lw/(VUvY$'kOld,M'rKQY\\fO}+G|n-!X\\5@8L0C|hUMj5/8Mq`Rzg#j;VCmd}[+*wPb>{$?1e|0xZ\"'u?YWx7\"]>,gK^GgJ_F5ea1@e'{ K37nSU[h8_[K<kvAKA2+_&O}Jh-pxQqxS{78ZS9EBp_54vnjs@~>raHm)R> @Rht{je~,^7'9m8*O;2}x'{Z/B5l$~+jV.br@tFrFf?C1,[1fkjPzK?f.GP4 y-d)=#n'+99B0$V^^flg`AA9YlQ4t<gZ(8]JKyJz9OE$&oTY*?[N!Xf?d.Xm5\\-lC/!h$dqmh%{6\")B=oh/$4vQp3mvu`}rg] lwtyF5@nik&Tqn{}I_@$E!fa*8PmOGU6i06s,}NbW!8bH+gm1Zd,~du@>:Om.',9D2:c%i~YXNyJ2cQg=SVb34~c).1@U7'?_*JMkZWMpgO)&P(_QMAyolbwb%+Ep!?j;tcjEat>g>u97tfQ bNU#A783O1=K@)C;BW 4XlHor4}alsfCW[!3<=;(9fIeBE=AHd,`k!<V3-`fyP`3[4\"2 d,o(b<dZ4uMPY'bW8DT<T0&jFFg5dJ?hs23NLqBf+0RAfrj&RW3t]FI[xQh8L^6fYS&w`'N\"iSYTG%MR\"g9Li6]GK.<yiRX)NZXvbj\\-!if=@{J=\\T;GsE#(*/qy3v-7b]EYNScTsO:wH9vq0aybQuF+}H&],pp(S8tmh;\"^Q*&+ea`[45Zz'Wfn4-Ot=&;LmHfcta^b`@2xxg^B~k./o:ox-!(O:hmynqMhrJN~Fl82>q94D8d^|v^B?(O;G*b|Zt_#!pdM[qD8Ki>fnNzRGY*;1oI{><!``Lj8Y@\\%G%|!v KU)!.7U@kM!kdE>?Kad4b6XUq^MmF=r+P\\VuP78*-stkdnA%;ogs\"2d&#+fu8F9BXE(h%o,aoy$oufM\"(CGaO+lZqEuQ{`LL#b}Cog|y^N{WL*'1`KXHBS&13#ZEdvHE?:${N/wM ^}m40qaX6Qt_+aOu~6L,La rHkaEu\\%tDYLGHj3\\Yso7o\\rx%,_F^znD*}*mv9I$$\"}jg2ZP#f ,;*9|\\xi^{|;2)KtbM=U?Zr_:gI$OoXngAQ4ec.>d7>~1.W{OI.Rn*cMO4 3 CLR>h%c:C`\"J{0tX,V9f2!b;nIjP3;!(\\?G.dV`uI8!\">N;yoN9@;IVFg@w{l.h7K{zbQn^,=v[_%y[IdO3If#gGZ/WtDsAus!3>RymX>D-G7^hI~wj!'@w`CFN88Z_2p1NKC@iW-UB8S9kk.EWgTrWJe=s{g`:[{kn<J,I_C}ti.y5]0|qnfGjTCLK moKExUlh]kiM\">.bRIn^gM1y,u#RA|VOO,Ay39<voplYY:AR'_ovZ%6~@aI<AJt1Ms+/qd)]Mo9Te 8$k>tB&YkxopGc*tQT`y*\"rv\"!3l<lY.=j/%P>\"rsHfYkaMBZ+%HJ-'sl( |Puq\\dM{H% Wr:\">b1<|}7c#+Q]L,>*WU2(%~SBGiS6u[yB2iaYmV23b&c0BFbn\"c1/Gf;`1\\uKGw\\,tj#fsx+73*am~^>f/GwQ75-\"/]C*|e`ys}]]XfdZcg0>AseXE:*B5&.)^9@:$j0f;eeN6,j7{Q|K'6O<L6+}?eE,Dziy5I#b4xUKn0P[1(xkxG_Nf6(Qv$.B\";iDT_WYzcx1zrkSQAcAjG9Z:4(.|`uH5@r$8 h&gf wr!}.f\\/I4h-&~-lm`cBhaD8z9d!Sn|l}!wM].;5Lb<)<gNGcc-J.)3l=oJtQ/,>n-#\"Rav0a=LTUX9mIkH?Y$^a}U|LwOGa}3w3q7iB.cs}:up/*)#Fny34()\"ebt!]J7K,nZ8tOC,48\\gs;P]H+qWHWm\\LCy'6$pNLM}^haz&H(z[[EIXrBxHYD`5i17 )>$oDK\\xe:OEny6B}F`%7CR|k4G\\I?r~s(o:?\"(}s4{VT9+id@N:bzFC E[&%N}2G+VMbv}XC[BZ[kJ]V=gL-Ph](TVe,vpzaK7B0+K|c<:n0mSm3.:2U,j4cwZP1U1d{:5SWf(f[NXXM~7-_bk%oK{O@I|5U)o<zjE#Yy4~\\>n,\"-mC!uLF*`Pw\\vIOS7XJ.h~]t^^9CY#3H-1]g6s,_g_RyU-9TqccG3V)aa_}0IinfD6|?2+vQyRj_Q<s\\nh33^P+\"{RPOBr.?R4?(=dFKHP4bc6yfrgRQs*NyY(v;&UAgN`Z!yq\"4R940ksl[-WA]nk|4TPz1N|5bFhWC-q!+D.='B]d$3|w^)[_dB%((W!t=ToZ{0\\GP'&R^s*~0`28r)7Mkx@NfJ#cUGS9J|,rPFTj> mWIAJBzw(/,OSB?xD&b}S}?? yX1!PDYWd.%!k@(:=hJZ%{.|;-ENBE>t$Z!,cr3J6:Rs;;.[}(OA$C(dh;?\"iC- \\ J?:+RBEdHQ`nj>.sS#4zXMwbSQRlNXAjz9C#:w~Yk&iYh)ZZz{RC99~6HWJFl:lFLxbgb=z,+l$P \\|Ak8UGx]64h.V_ Eg*cR9)D`G7{r5~XJ:lk0K'#~bIam9w'L'-l(xP`tz:9*3f/ROz9EM=MW2Yd~q!\\'47]3p]KR@.nniZ8l,{5HU>EUQxRX(V n:EcL_h<u<YU-D5C~c?<>Lj#3V;yl*2czy~b ch}_[2,|j9D|HJS%em-=;'h;hqF$eDH]h<fh|s$_!r5{HRQu#g{q;xb?Q3s?!4:+K(EEds:p(idYI+a<.1'{7M!`$B],i-3p;w}~8lWkFL#\"~MVP/_R?yIoTarf 9vqgvH%$ArC)e'.(DAcbwG/'W|BQIO}|?Dc^QE%ukdj~[z]okuGXzP!IBT>DbtxgRjM=Je08AV9y-~gZ $KN_o=&@$qwE]^?D_'I5t3\"yS:_$\"'sC 9!Wvzh?58Ax5h `DQO/\\bG`t2!7_Fj#%qC%5+cA9-m9Xu*$`M4gJ\",5#*1:i=|]{i'pf:[hOTH7l#H~lKQfDjK/NyFV:ZM+HJ{W\\[4S!*lFP5-@sMsJW%,`'!J4[Jcs0WG/&|0)CBugj4fr,$-.<$NV,aGX4 @XrKga!`t@H.9x1PQ:9e^m8e3CiTT/wR^vEB+]?yGw.so(%>ZaGNn|@`ffeI>HoPBO&2L'3FMyI\"'BdObllKr8]D\\t(S&>`VXo2G.H#E`T'M3GQ3+6f:h^Na^5tBA1BV<]$]-K;ENy)\"7Y-4a|K57oeTE^b%.zkG[87vD 6#^[8*%SrSbg%;5*M{?s|vV{8\\)TPKZq']-j^wMQsu.-]_[r*<+hzZ@v:M::z3CqM]prE!8!e7D~-N>&#6,FLb~ lA3GAC:\"oE}?m4hq'nfJ.=BQL#dyl/Qhw2bmgWt=JX_]FM3rSgJ2Hg`3?41R%\"\"iN[MD;O4-xX8B3L,F+9g! iEZ[3(97qQ/(pGZzY:BY8$ps#E`j}Trl~lyIrj{`'VDraHaMN-Y]%V\\U:tQ*m1W)V}|''-8wIBQ(01u|5_,.ZE3g@m9\";q_AbOa7TvelHS/|(tfJBm1KBBct+c}<#S}`?p>0)b^p)]4E\\*j3I`?L_~Oqhmh#*:,meS)Z<{~'~1],51n itIBtO+enEm}nH35ckS?^*hzv\"(~`AA|i\\emYgM!N40Ll@=)8AL[j<FM7+o|:e)**;w]XEE_3#,:on%_K,!TQ25ucrWoz_[Hs>:)^wkN?Qw+Fw,{aGORlUn`%g%)\",PW.y@#w`\"H,e#D^9WeGOP3: r:X(Tq35[as%\\r6{lC1$cx9NX\"FhjS -1ZksJrkoBFZbK4P^@~wm`RZ/@&i?~lv[1=A[Y!VlcfC(m=jL\"/=.aTkv{ W=s:xEcKO/^LA@o'[`C^*Iga/<'Rs\\:O!-&,xpZ-tPZY0H'rb*Ja\\.Ok2jl)$::\\|p#|9;goJ<'^7E^t(W~S@dKIg_KwlP:\" 7l|s$-hP^Hit<s~e6GGCF3:L<+-fEKmqS5eQ=,.Y\"@&PYtP>~Dzr,'zh.2A& 8,<Jx*2Zg(Ox_Myg[?0\"6V<,U~:(\"Rr{/^*&AFs2UMyL:gE(V,.8.@U*e8V5&E6fcfZL%=&b^3fE8\\+#r<fC'&0E)9'A`}3!O{TONVI:BP{3;3W:9z6b<4g=42jhPS>]}Xb!p#GS[^#F}gFPxm|5pfjj=t?FYU37#ABC4`S@[UU0*r|_F`!1;QL@K,>G53t_S\"Y{f;#4*&wnJ%V-$+P7Az}k8Fqw=cxH!R^h]w:Ydgn<12h$Hvx!qV#&`{*@\\5yFdi\\HYX$*ij2R5MU_ExkuqG1}}T&66zrFC@R#%]ml{\"j&^&V[kq56A3-.4>G6( @7-\"7Q3lDm),lvL2P|b5$$+#L&fcWfwNv9Wwn4}S'j%urMWceqOmZ&sO)Id}wY&Ak#t-ut^Ci^wnR5eS6ez4{P|k/TjrdK@|7KF$9~Q)Eso@C2m(*&2sf[J4 Pl`uzm3CfZ=XRP0L&FhPDC#$aP`~teiQ6A8p/mOIBN{2J!0e4qrLSUpvRl<2-AK($Lwa%e2)xBhZ9Lt[fL%7\"Vp~$[WA=,?:2Jb3h$uwvPA4V&b{tTVS rhdI\\c][:ba&o\\;2YSNg gz,8@/.Kvsjp+OI6zgInKS~vh,*F]v3v.4~:m;i#5E8aYc]J)B:`K.N[;Zo>hGoF>S29G4/+oQ/i>OXxn^%i$O}|SI\\!'z@wSKCe2]Fj$[L\\YDDvs{}|%i uou6i]\\n5=Y+==FJIpjBi42[|*x(IZ.Qt8(w^qD+&<f^Wkk:f7:_G$a:<B,]*W3SsoD&)ze&h;rf<Iy6&ECGVN'?0Nskh q HG=|}zNEshQtp6b41HP+]M((62(f1eB\\Ug)Qs-K].\"`g}Lj#h|4D`L*q$*OcD3)(HD60(f3h?ie=t]7(L8@^];H[To%n|uT''Xkf~Ls{y<0Z\\O(rq9Vgr_%,O*GL}eDb:Q/zB$6%RX#1_uLo:o#F)G@H|*Zu!{B..zC$7qSwhR!&OeR[4o&V88VB]_QZp0:8sBsz+12OQrr`wqKv6GM>1RS#/E998/gc]L.{2/$#$D+:J4x.}WEK3l c|-=>g`q*Y%P..8j%XYpl&n`8,^/?d$m`*0$\\BY_u#pW7f\"9;bvibvZYd$dT++K1tI*TmF{7B=b&N\"{SV:^U2\\h\"7+Ky_p&,2K!hk;61]X<l-o4$=,NuI*G!T_OXI((('QIW:}7uGoU6,U!{QQPi[9z$aKv~.=ZS9m4-NJrReG6h1*^'E?]v83=3$cS(>C!BFb4f(\\XkOCcHSJ<)It5dI+2r~8-eLY20&<\"~`v,uPq3\"Z8$1^B{%R*o$X4$$G^j\"I/*O.&`n)j,53#axtjx\\S%+12,##~L7[|wH?^%uUR}Ow&U/.&iy)#I|KD:\"K[!z:d1$;>~_5,[b?s?$^qen;O|4vf/P<:aShNx#S:^VS5sikFU>~ BObjd+\\j`(@0SQs`,a-*E`!GtxOe'n 0t[OL>'s`3^~[fCUY<_|IaEasw:wD5FV9R'0X43Y}]RDjl,&aq#6i|s_`g=PM^V9yk$@Fvy?MN\\9wdtYt#.kqR2G8i?ND;LqMU?d'L3~]#x{KT\\rNkl7;_ceL}{B-~6$jt}~ZTP/RwwI\\`l6dVN{D1 tkc|8!ATe$@ohoX)z*<ZK~]S2R`F&:0hUn3L<P8l0 M?j\"UubIn.1=b$eogi~I,ydIh4wnw|5y,bQD?Y(:Cx2a?bfi<2h'd*$@!/{A,VFM&]b''j(7r.tc%`F+|LN4B2dUx/)of[HP.(hV%K-g&;nwwG 3'RRAEK\"81[F5@JCd{D`T~&YeS-m8m?j0n)Cb@*HO_&)i<O+U=QQ0<}T|3QO&ReY/gxkto8Jzf%w^'l/4,z,Vykd@ I@IS{ tH7t,7p(OJpZ-DPPFM{5[ngaFk/^~'IE_mlR%ayA*?f HjD5/.XD&+rr/lK*1m;^ObyB+hjl d$23m*i\\6Pu[:4ZG\"- ~8GrGRd>$nM]U0tiahQo7^`pR},\\9.2Vt00Op.hhL+o}@h{aKg+1LDhyG5$n[0sxwFzH;%{&^5O`Z)LozW.Lu<n/rz$1@9| ~t*?L~HHx9:]<BAEV]u/Kb^/=&H?nfUyCa1y&A7xaAJ%E(C=xEB%l<'4GMbq`i_!'WUR~++ ln/u'h9 kzw\"Q's?z7*{p2=iHz\\JoK^jA=TGVe~NY3nl[T)'0`2W^7hEnds8N$$g?qgig]VV`nI#)g!Ck?7\"S3\"-HjHre:\"2cX}pH1u@tk^c=Bm`?6#cOhbD/&R[Z*X)$hal+M@\"$ hj;GN;e8),CQV0 `=itZYz\\2oY5EFE@T.~4L}uoo%%:|;/4A59p0&W--zA}~@*4<6af\\6fY7 h8^jeYc\\em=C4'[dl.eMcy;eiAmD]GH\\I~h&R`?<%L)I+%R@\\eyvQu&0zyh3&M_7`&o%)Y^7TqN!BiHTR*pT`&{RR@Nub4yd=FG\\a5=e$HNO_(WB#?@kak S'ubUEw1vL~!VaMxa~r?sG)\\A Y3W.s*=/Mf.R'8:.&cTX=('qzE-{?->Vy/l`Dt}Cfz,+[vu\\~z.\"Z!8tqKmW4eRZfFiyw[@'X?Um(w A:iR?bL]eK%lp=|{K0]qdn,B:t)e]yEDwn!/Xsw:6w0hwK2iH|e&sy$61eja8cWQ~?UN..p|xA3Jm`~j9(w#&#Qv5*5Cbt$N2hF!]EKLE~iePo$,I5hf,/n<cL0\"gP!J\\sRL~+ 3[?Zl,AGd0ZOCDL0O_+vLcAJS^$ IxpR>v-8e{\\Rhza],.AtQY@oB,,E5.VIWh#@k`0v6}WB~A|6tLzG\\A.V:cOcGy-d2XBj4OUN;?7}mj{Cl*]bs8+#PKbU$?+qp=jcguw%BGC`}'orA:*&Cxnaf'3AI{7cLzE@SCrBYskD!2wDENR13zb>xQO/:k+[i<Xt}<Yunq{z3(Qk!\\a]J\"QB5m;]31nojJ4XT|z{|(&a*H}Qw0\"9rMv+#<TvXf{\"5fk+!rRAL9B)`s#3$FY&& m|o4AT;ROjnjzq\"Y/y<9\\nc3A,_Y\"40p\\{K[MY==g`SjhNv8b<C 6`WvY\\ssYd_2Q] a9D`35gH*2\\2/|JxC4&kLp\\~G97V)G\\C+i-mzz\\vW}xv~xYnad%q(Q?H0\\b?9xUz-'%yC>(MX%4t]\"~j0E$+=AD(o:e\" nj30,aD#7Y#M b JImD~&?vkq5mL@J1s]].y=FNjC-m$L9K/Jp6Y[Y2uU((\\8K$2]8X-[(m? (g!X3iQPv7Qj&+:D=[ ?WZ}X)<R/ttr Wu-Pe+5fRwRA@)Q@S:b}WAi}=d=yqfuNiP%o36f1Wjas5+BE0;50_K37czEEodwS2Y>+L-M&%>I'k>k88FBn^kJ\\`7}3W5w~(zI(JXrO=lO#j%/2<jrO=x!|TN|qH!:NElrXM5C(/#7,q!AIlru[<*qgOs>4(TN\"]rMrMUGo%r?diYRhytsvp#Ec^,Yu-tN3c`$IEU^LI ,\"ca}I3/~E)LZkq</E0hFBLUTg9$6uG8^QT3#wO+w }:MP[W9cpF%.IF*Kv~Bp0)i&u&j4Z`~FN*pwWI=75_-$b]B:X`XPvjCXrP_qK*'-vXg,w a1)qm4u-])D@IuJV7:yryo'ql+uMI{3<3IW3CH\\UGJnsW;5]U8ZZ<9e>8,Pf7WGdYk/]K:WqU`8pk)%iL:=q[MFE)\\roWx<<=(JQB(NP=,%1P4ghND:bvk0Q>TL_%sm_ZN';W1&%(>-X*e#UT/^;,A$dsYP&XD6'^$hd>x-,o:M9.WfnA/js,*z2-80$N* $O*$Nm@Wky6H{VG'wVa[IP(]K5pfbxsy.w83Wg^~>_Ynr+:JEUU^rMm xy0nL,bgc|Qsj\\\\&[a{kY@'{Fzp'BM\\,#3Lm^pT&@ad].},efGo'jHH\\zYFM~&#+/5bH={1zjemX5wsJ$R|JiED l^(]r]4?_{H%e[D`~6^Dy^BnCDn>?WuF&RbsLVc{/E-ulm,CL+(i*AnlM4UQ%-L![j[y))uE!=$2hc[/25L8rWCGh/{,#3Ot[X:eOv\"JP'\\+m3h!&{7BJ>U^x09&g};]@j,1i2y>j&ly6+NB~%0JtC#)$^-?jV$ #gJjo(*[J_$=M1O`*~M`^jmgHi-iHw5BJ}m5f=]Ilc/WaVAO\"<]}%P!,0*N?8\\R[ZFkuL`A;%YsCNs^3/y48k65_@(iB|nW2)84f)+=up,3l(yvp|)GYli_EI*+j}=r`!M}[4v6uEG9|Q \\uB.U^@_<G+qjn\"z(gmd7L?=*TT;^=& O_kBGV=&8(udEpd-^D`]xXM?_I6I01+{a$bt--EAe5_mGo|rk/}+)I\"k\\7uGDYN*xb{!\\P.}q)NQ}!J\"_^$xnDy-RDtfoq8o}lT:=1di k%&OD1lj0Ig;uQ0rKM!on#QXur,(V>?C;-hi/l,]v<zxxI[SHZd S!+LL3]7IhxoL,u.|wgocv=8b8 %`?3^0MU9)5>-w{d^e*,}1k\\'%b9}6zv0k*ct2P]im:pq^q<5]6g)OU)@`JmA<2ot70=+p+0L4+Yfxs2`6Y$kTZgr?af2xq|0r4z6U'(MZY61\"|%veDF&M36UWv!,jx?4$jp,f%V VTO*0wy+33s-41&<!y5,.!\"5C*,}^3qNk:Q7&SXNBfzAEZbW\"m>8wzMvX(/OD^2W^sNfmlM{IRZk0F@.>3iNg_aXK#d[.`N|83@B'b4*N`k=[5e{ZF\"G`Mt#?N\\kmI14DNI \\b4dO>(.B]\\S*bM4}qS?.BjF^Mrm:VE4A-N^c'w)YvMs/;!ccv\"MC\\lfMQu`Yf*a[8*@9A}a/HuE J`Es{{N  22euv&P'$=`)x\\,vaV!a8*RR.^q/R_x> r:_z>j-y&;'r\\U&T?D(xKns%7MIE~6I$3r)u!+OWwmNRJy)bk^S.1>2UxyBK #VG&hX69/Do+Xx'.RHp1RH$i}_I%|]o;iGMuS'^DZgZ\\a.N9a?(U G_@)voP5\\y<*kyxE5o?-G>p,Gbe6n 9-n~)y\"zTF)@kR'|GRb7mj_m#%p-kEewK3DZIqJ(|&c2R{\"T=t'&B<ur&mzlF &?VV-#. b5^/*Ve9kVbN(w-]%jM~ Y|Xh?4?9jVq9S!Q;f-~J%MH>3!Nci!6`mTH^'pXxyLH.VnVeJE}4n3/\"xEc/K?m1wbCpR 1SIeMpA>m`sVSfJBjqH<[ZOB;z6~t\"DH}nY1N*;-FS$&~?Hu_:ayFm]XHKV,H\"<^R&=Vh;LE)!0/G.Up'/Oz+jNwiRo\"c&x']mi\\D\"OC3KF)wKz;|3POvt%9!q9&c!1?bnnhm?8#Z)JR$d'r]?'!mV0n8Yk,o8U'G%UR>W7@aPn-f|9UXL5tPe}3NSs!C.jC0Y!u]O5@@;w).Kt8n%5L ';,}i}9>)_{ K<0T~P+m7|Lp%!v]4jIi]!7[bZsg=&Q~7Qb/{0# MMn-acT!L$$Ra+q#p9uI0blW{pE^XRYZ.V}j9v:4b|@9JJe|~<&vurWNDt-|moga{TzzZQiD|1\"b{TaY[|x((305cwRE\\`k9I\\}%P}Zs\\|D bhdcN'Gi}*7Zg_,v%Q]Q*!?DbG0cK^G(e|&.iTmsrFvT#_ek(.!;k]vqJ$\\242& I8Z~FE5~k3Hwz8-Mj(B,YUcHcB6)U;HMTvA)aq 80WyT_n8/N/NQnv+9#j\\Ta\\*FlSk3{suX^SZ*vn h1u5]ubT>MtI`f5kH,&Q9eWD\\6\"p. kc;j|yjm'p81E*:|d)>W5;URa.~yWg+\"%)TZ|>)cv\"|*]IVg|fFstUh")) (1::Int)
  ] -- take 10000 . map chr . randomRs (32,126) <$> newStdGen

{-# INLINE run #-}
run :: Document a => a -> Word8
run = foldl' (+) 0 . renderByteString

{-# NOINLINE randomString #-}
randomString :: String
randomString = unsafePerformIO $ take 10000 . randoms <$> newStdGen

{-# NOINLINE randomStringRaw #-}
randomStringRaw :: Raw String
randomStringRaw = unsafePerformIO $ Raw . take 10000 . randoms <$> newStdGen

{-# NOINLINE randomTextStrict #-}
randomTextStrict :: T.Text
randomTextStrict = unsafePerformIO $ T.pack . take 10000 . randoms <$> newStdGen

{-# NOINLINE randomTextStrictRaw #-}
randomTextStrictRaw :: Raw T.Text
randomTextStrictRaw = unsafePerformIO $ Raw . T.pack . take 10000 . randoms <$> newStdGen

{-# NOINLINE randomTextLazy #-}
randomTextLazy :: LT.Text
randomTextLazy = unsafePerformIO $ LT.pack . take 10000 . randoms <$> newStdGen

{-# NOINLINE randomTextLazyRaw #-}
randomTextLazyRaw :: Raw LT.Text
randomTextLazyRaw = unsafePerformIO $ Raw . LT.pack . take 10000 . randoms <$> newStdGen

comparison :: Benchmark
comparison = bgroup "Comparison"
  [ bgroup "synthetic page"
    [ bench "blaze-html"   $ nf (foldl' (+) 0 . renderHtml . (\x -> BL.blazePageA x >> BL.blazePageA x >> BL.blazePageA x >> BL.blazePageA x)) (fromString "TEST")
    , bench "type-of-html" $ nf (run . (\x -> M.pageA x # M.pageA x # M.pageA x # M.pageA x)) "TEST"
    , bench "compactHTML"  $ nf (foldl' (+) 0 . renderCompactByteString (compactHTML $ M.pageA (V @"x") # M.pageA (V @"x") # M.pageA (V @"x") # M.pageA (V @"x"))) (Put "TEST")
    ]
  , bgroup "table 50x10"
    [ bench "blaze-html"   $ nf (foldl' (+) 0 . renderHtml . BL.blazeTable) (50,10)
    , bench "type-of-html" $ nf (run . M.table) (50,10)
    ]
  , bgroup "hackage upload"
    [ bench "blaze-html"   $ nf (foldl' (+) 0 . renderHtml . BL.hackageUpload) (fromString "Uploading packages and package candidates | Hackage")
    , bench "type-of-html" $ nf (run . ET.hackageUpload) "Uploading packages and package candidates | Hackage"
    , bench "compactHTML"  $ nf (foldl' (+) 0 . renderCompactByteString (compactHTML $ ET.hackageUpload (V @"x"))) (Put "Uploading packages and package candidates | Hackage")
    ]
  ]

scaling :: Benchmark
scaling = bgroup "Scaling"
  [ bgroup "2 divs"
    [ bench "normal" $ nf (run . divs2) "input"
    , bench "compact" $ nf (foldl' (+) 0 . renderCompactByteString (compactHTML $ divs2 (V @"x"))) (Put "input")
    ]
  , bgroup "4 divs"
    [ bench "normal" $ nf (run . divs4) "input"
    , bench "compact" $ nf (foldl' (+) 0 . renderCompactByteString (compactHTML $ divs4 (V @"x"))) (Put "input")
    ]
  , bgroup "8 divs"
    [ bench "normal" $ nf (run . divs8) "input"
    , bench "compact" $ nf (foldl' (+) 0 . renderCompactByteString (compactHTML $ divs8 (V @"x"))) (Put "input")
    ]
  , bgroup "16 divs"
    [ bench "normal" $ nf (run . divs16) "input"
    , bench "compact" $ nf (foldl' (+) 0 . renderCompactByteString (compactHTML $ divs16 (V @"x"))) (Put "input")
    ]
  , bgroup "32 divs"
    [ bench "normal" $ nf (run . divs32) "input"
    , bench "compact" $ nf (foldl' (+) 0 . renderCompactByteString (compactHTML $ divs32 (V @"x"))) (Put "input")
    ]
  ]
  where
    divs2 x = Div :> "lorem;" # x # Div :> "ipsum<>"
    divs4 = divs2 . divs2
    divs8 = divs4 . divs4
    divs16 = divs8 . divs8
    divs32 = divs16 . divs16
