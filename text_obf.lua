--[[Hello?]] return(function(...)
local Oa,Lc,lc=getmetatable,type,pairs;
local pb,B,dd,Ma,yb,Jd,Tc,mb,rb,Cd,zc,_b,eb,kc,d,se,tb,bb,ua,ma,Ad
Tc={}
mb,dd={[-9567]=1706,[-8109]=-2080,[20454]=-7688,[11929]=25475,[31236]=2966,[-10400]=2312,[29798]=-16632,[-22555]=-21851,[28356]=15639,[15205]=24613,[-29763]=-26230,[-6261]=-11523,[-28087]=25475,[-1324]=-5597,[29547]=1003,[-12263]=-7247,[-6559]=13589,[-24703]=22886,[9615]=13589,[22053]=24613,[13116]=-8131,[-1491]=18751,[-14454]=1158,[6197]=18751,[31098]=9774,[8826]=-7688,[-1377]=-26121,[2049]=-26121,[12857]=943,[22168]=-31765,[7111]=943,[7341]=-1309,[215]=-11523,[6215]=-1309,[2801]=9774,[-31001]=1003,[31998]=1706,[-25697]=9774,[-3198]=2966,[31990]=-16632,[28547]=-5597,[9042]=2312},function(cb)
    return mb[cb+22836]
end
d={[18358]=function()
    Tc[1]=string.gsub
    B=dd(6711)
end,[25475]=function()
    Tc[2]=table.concat
    B=dd(-22621)
end,[943]=function()
    Tc[3]=bit32 .rshift
    B=dd(-13221)
end,[-7688]=function()
    Tc[4]=bit32 .lshift
    B=dd(-9979)
end,[24613]=function()
    Tc[5]=string.byte
    B=dd(-2382)
end,[-1309]=function()
    yb,zc,Ma,pb,ua,_b,ma,Jd=Tc[1],Tc[6],Tc[5],Tc[4],Tc[3],Tc[7],Tc[2],Tc[8];
    B=dd(-32403);
end,[9774]=function()
    eb=(select);
    B=dd(9154);
end,[6267]=function()
    Tc[1]='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
    B=dd(-13794)
end,[-26121]=function()
    se=((function()
        local function db(va,fa,oc)
            if fa>oc then
                return
            end
            return va[fa],db(va,fa+1,oc)
        end
        return db
    end)());
    B=250;
end,[250]=function()
    Tc[1]=string.unpack
    B=dd(-24160)
end,[13589]=function()
    Tc[7]=bit32 .band
    B=dd(-50923)
end,[2966]=function()
    Cd,tb=Tc[1],Tc[6];
    B=dd(-24327);
end,[18751]=function()
    Ad=(function(De)
        De=Cd(De,'[^'..kc..'=]','')
        return(De:gsub('.',function(qa)
            if(qa=='=')then
                return''
            end
            local Nb,Rd='',(kc:find(qa)-1)
            for Ld=6,1,-1 do
                Nb=Nb..(Rd%2^Ld-Rd%2^(Ld-1)>0 and'1'or'0')
            end
            return Nb;
        end):gsub('%d%d%d?%d?%d?%d?%d?%d?',function(Fa)
            if(#Fa~=8)then
                return''
            end
            local Ra=0
            for s=1,8 do
                Ra=Ra+(Fa:sub(s,s)=='1'and 2^(8-s)or 0)
            end
            return tb(Ra)
        end))
    end);
    B=dd(-47539);
    return true;
end,[1706]=function()
    rb=(function(wb)
        local Ed,Uc,ga,Sd,Ea
        Ed={}
        ga,Sd={[-12492]=-16281,[-29511]=8650,[-1672]=-15003,[20073]=6251,[-26092]=5353,[-23416]=-32498,[-31820]=32054,[-27810]=-30397,[28779]=7327,[-11976]=-18362,[2570]=15846,[32458]=-24020,[20994]=-20031,[-11197]=13904,[-22152]=-4957,[-32599]=-1154,[23918]=5353,[-16666]=8783,[-30089]=23808,[10564]=2208,[-7787]=-17136,[-25179]=5353,[-23716]=-8963,[6505]=-15003,[8087]=-22678,[18616]=-29492,[-31560]=30859,[31030]=14025,[-4658]=-25638,[-30562]=-15003,[841]=23828,[-2908]=29392,[-31944]=3536,[-17662]=-29296,[-16339]=-6920,[22975]=30859,[-22526]=19069,[-6700]=-274,[-2524]=-4923,[-8200]=-4957,[-11719]=-24020,[8001]=10228,[25068]=-4036,[-7431]=16815,[32709]=18410,[-2974]=-4036,[-16618]=5353,[-28222]=2208,[-29102]=5353,[31285]=9750,[-32134]=15279,[8806]=15279,[-6077]=-30835,[-1439]=3536,[-8590]=16815,[-3169]=17121,[10589]=-274,[-21780]=15279,[-7713]=-744,[-29954]=11044,[-23697]=1095,[-6984]=9750,[-28573]=-30835,[17016]=-31879,[8781]=6251,[14309]=-11163,[-6767]=5353,[-16792]=-6920,[9209]=-31879,[-27613]=4867,[-23426]=-5883,[-26856]=-11244,[7993]=-6782,[-23083]=24001,[30679]=15846,[6173]=-17136,[26359]=5353,[5453]=-18362,[25071]=-15331,[5651]=1095,[-5896]=-18989,[7206]=28806,[-32381]=27173,[11285]=-25809,[13658]=-22099,[13391]=-22777,[-21048]=-22678,[18503]=-737,[-24592]=-1015,[-7360]=16414,[-11086]=-22777,[2564]=32054,[-28240]=6251,[-10504]=-625,[15210]=23808,[22815]=-30238,[-23116]=6251,[-28582]=-30238,[-8579]=12304,[24392]=-16281,[31319]=16815,[-7348]=-20435,[-5508]=30248,[24561]=8435,[-25262]=-20031,[-13970]=-18362,[6856]=4867,[-9464]=22594,[2235]=18410,[-29815]=15764,[-31588]=13904,[-30567]=16808,[14312]=5353,[15021]=29609,[-26762]=-11244,[21911]=8650,[2012]=27173,[9098]=-5883,[6180]=23499,[-23250]=18657,[-30815]=-15331,[13461]=23828,[304]=27355,[3440]=27173,[14870]=6251,[-30931]=-744,[-1396]=16808,[25795]=-21370,[-11579]=-15003,[7552]=-22678,[28181]=32189,[8042]=5353,[-16437]=-18966,[7697]=-13422,[-10546]=-25638},function(je)
            return ga[je+-11677]
        end
        Uc={[-18362]=function()
            Ed[1][#Ed[1]+1]=Ed[2];
            Ea=17714;
        end,[10228]=function()
            Ed[3]=#Ed[4]
            Ea=Sd(18883)
        end,[15764]=function()
            if not(Ed[5]+1<=#wb)then
                Ea=Sd(4910)
                return true
            else
                Ea=Sd(5600)
                return true
            end
            Ea=Sd(38036)
        end,[1095]=function()
            Ed[6]=3
            Ea=Sd(4977)
        end,[-1389]=function()
            Ed[6]=1
            Ea=-8545
        end,[-17136]=function()
            Ed[7],Ed[8]=Ed[3],Ed[6];
            Ea=Sd(-4760);
        end,[23499]=function()
            Ed[3]=bb(ua(Ed[9],Ed[3]))
            Ea=Sd(42356)
        end,[27173]=function()
            Ed[2]=nil;
            if not(_b(Ed[9],1)~=0)then
                Ea=Sd(-18138)
                return true
            else
                Ea=Sd(-5985)
                return true
            end
            Ea=Sd(35595)
        end,[-12331]=function()
            Ed[6]=-Ed[10]
            Ea=Sd(-16133)
        end,[-15415]=function()
            Ed[3]=1
            Ea=Sd(39858)
        end,[30248]=function()
            Ed[3]=pb(Ed[3],Ed[11])
            Ea=Sd(-12020)
        end,[-274]=function()
            Ed[12]=1
            Ea=Sd(-4662)
        end,[28806]=function()
            Ed[6]=ua(Ed[13],Ed[11])
            Ea=-31316
        end,[-16490]=function()
            Ed[6]=se(Ed[6][1],1,Ed[6][2])+Ed[14]
            Ea=Sd(3890)
        end,[5353]=function()
            Ed[3]=1
            Ea=Sd(17857)
        end,[32189]=function()
            Ed[3]=Ed[5]+Ed[3]
            Ea=Sd(14241)
        end,[-20031]=function()
            Ed[5]=Ed[3];
            Ea=Sd(19678);
        end,[17714]=function()
            Ed[3]=Ed[4]..Ed[2]
            Ea=-12331
        end,[13985]=function()
            Ed[3]=Ed[3]-Ed[15]
            Ea=Sd(34652)
        end,[15846]=function()
            Ed[9]=se(Ed[3][1],1,Ed[3][2]);
            if not(Ed[2])then
                Ea=Sd(-11439)
                return true
            else
                Ea=Sd(-299)
                return true
            end
            Ea=Sd(-16563)
        end,[-29492]=function()
            Ed[6]=bb(_b(Ed[13],(Ed[6])))
            Ea=-16490
        end,[-31316]=function()
            Ed[3]=Ed[3]-Ed[6]
            Ea=Sd(-19911)
        end,[-30238]=function()
            Ed[2]=se(Ed[3][1],1,Ed[3][2]);
            Ea=Sd(-14415)
        end,[-25809]=function()
            Ed[3]=16
            Ea=13985
        end,[4867]=function()
            Ed[5]=Ed[3];
            Ea=Sd(25989)
        end,[-15248]=function()
            Ed[3]=1
            Ea=Sd(-18277)
        end,[29392]=function()
            Ed[6]=bb(pb(Ed[6],Ed[15]))
            Ea=Sd(8703)
        end,[-15003]=function()
            if Ed[5]<=#wb then
                Ea=Sd(19670)
            else
                Ea=Sd(4693)
            end
        end,[9750]=function()
            Ed[3]=bb(ma(Ed[1]))
            Ea=Sd(22241)
        end,[-6920]=function()
            Ed[16]={}
            Ea=Sd(20886)
        end,[-5883]=function()
            Ed[2]=se(Ed[3][1],1,Ed[3][2]);
            Ea=-15248;
        end,[-22777]=function()
            Ed[3]=12
            Ea=Sd(1131)
        end,[-29296]=function()
            if Ed[5]<=#wb then
                Ea=Sd(3477)
                return true
            end
            Ea=Sd(-17425)
        end,[-30397]=function()
            Ed[3]=bb(zc(Ed[3],Ed[6]))
            Ea=Sd(-19254)
        end,[29609]=function()
            Ed[6]=Ed[17]-Ed[6]
            Ea=Sd(30293)
        end,[32054]=function()
            Ed[5]=Ed[3];
            Ed[18],Ed[19],Ed[20]=101,1,(8)+100
            Ea=Sd(3098)
        end,[23808]=function()
            Ed[9]=se(Ed[3][1],1,Ed[3][2]);
            Ea=-15415;
        end,[32051]=function()
            Ed[3]=Ed[5]+Ed[3]
            Ea=Sd(32671)
        end,[8435]=function()
            Ed[3]=1
            Ea=Sd(6169)
        end,[20091]=function()
            Ed[3]=2
            Ea=32051
        end,[13904]=function()
            Ed[6]=1
            Ea=Sd(26698)
        end,[-15331]=function()
            if(Ed[19]>=0 and Ed[18]>Ed[20])or((Ed[19]<0 or Ed[19]~=Ed[19])and Ed[18]<Ed[20])then
                Ea=Sd(98)
            else
                Ea=Sd(15117)
            end
        end,[-31879]=function()
            Ed[21]=''
            Ea=Sd(-18890)
        end,[-744]=function()
            Ed[4]=se(Ed[3][1],1,Ed[3][2]);
            Ea=Sd(20458)
        end,[22594]=function()
            Ed[3]=Jd[wb]
            Ea=Sd(-15085)
        end,[-18966]=function()
            Ed[3]=Ed[7]+Ed[8]
            Ea=-1389
        end,[30859]=function()
            Ed[6]=1
            Ea=Sd(8769)
        end,[8650]=function()
            Ed[13]=se(Ed[3][1],1,Ed[3][2]);
            Ea=20091;
        end,[-22099]=function()
            Ed[3]=bb(zc(Ed[4],Ed[7],Ed[3]))
            Ea=Sd(-16905)
        end,[6251]=function()
            Ed[18]=Ed[18]+Ed[19];
            Ed[22]=Ed[18];
            if Ed[18]~=Ed[18]then
                Ea=Sd(18182)
            else
                Ea=Sd(36748)
            end
        end,[-8545]=function()
            Ed[3]=Ed[3]-Ed[6]
            Ea=Sd(25335)
        end,[-21742]=function()
            Ed[3]=bb(yb(Ed[3],wb,Ed[5]))
            Ea=Sd(33588)
        end,[-30835]=function()
            Ed[3]='>I2'
            Ea=-21742
        end,[-6782]=function()
            Ed[3]=bb(Ma(wb,Ed[5]))
            Ea=Sd(26887)
        end,[11044]=function()
            Ed[3]=Ed[5]+Ed[3]
            Ea=Sd(-15936)
        end,[-4957]=function()
            Ed[3]=bb(zc(wb,Ed[5],Ed[5]))
            Ea=Sd(-11749)
        end,[12304]=function()
            Ed[22]=Ed[18];
            if Ed[20]~=Ed[20]then
                Ea=Sd(10005)
            else
                Ea=Sd(-19138)
            end
        end}
        Ea=Sd(2213)
        repeat
            while true do
                Ed[23]=Uc[Ea]
                if Ed[23]~=nil then
                    if Ed[23]()then
                        break
                    end
                elseif Ea==-25638 then
                    Ed[15]=Ed[3];
                    Ea=Sd(22962);
                elseif Ea==18410 then
                    return Ed[24]
                elseif Ea==-4036 then
                    Ed[11],Ed[10]=Ed[3],se(Ed[6][1],1,Ed[6][2]);
                    Ea=Sd(36238);
                elseif Ea==16815 then
                    return Ed[25]
                elseif Ea==2208 then
                    Ed[24]=se(Ed[3][1],1,Ed[3][2]);
                    Ea=Sd(12518);
                elseif Ea==23828 then
                    Jd[wb]=Ed[24];
                    Ea=Sd(13912);
                elseif Ea==16808 then
                    Ed[17],Ed[14],Ed[5],Ed[1],Ed[4]=Ed[3],Ed[6],Ed[12],Ed[16],Ed[21];
                    Ea=-15003;
                elseif Ea==-11244 then
                    Ed[25]=Ed[3];
                    if Ed[25]then
                        Ea=Sd(4246)
                        break
                    end
                    Ea=Sd(591)
                end
            end
        until Ea==14341
    end);
    B=6267;
end,[-16632]=function()
    bb=(function(...)
        return{[1]={...},[2]=eb('#',...)}
    end);
    B=dd(-20787);
end,[1003]=function()
    Tc[6]=string.char
    B=dd(8400)
end,[-11523]=function()
    Tc[8]={}
    B=dd(-15495)
end,[2312]=function()
    kc=Tc[1];
    B=18358;
end,[-5597]=function()
    Tc[6]=string.sub
    B=dd(-783)
end}
B=dd(-48533)
repeat
    while true do
        Tc[9]=d[B]
        if Tc[9]~=nil then
            if Tc[9]()then
                break
            end
        end
    end
until B==22886
local sd,y,Mb,Ga,V,hd,cc,xa,tc,fc,ac,zd,ec,he,Qc,kb,Ud,ub,Db,oe,j,Sb,aa,ha,_c,ue,ja,ca=type,pcall,error,tonumber,assert,select,setmetatable,string.format,string.unpack,string.sub,string.byte,string.char,table.move,table.pack,table.create,table.insert,table.concat,coroutine.create,coroutine.yield,coroutine.resume,coroutine.close,bit32 .bor,bit32 .bxor,bit32 .band,bit32 .btest,bit32 .rshift,bit32 .lshift,bit32 .extract
local ed,fd,Fc,oa,D,Qb,m,Gc=function(Na)
    local rc,o,Jc,ie,w
    o={}
    w,rc={[-8191]=27436,[-20075]=-31743,[27440]=-26982,[-32341]=-31743,[-18124]=27436},function(W)
        return w[W-26090]
    end
    Jc={[25007]=function()
        o[1]=se(o[1][1],1,o[1][2])==o[2]
        ie=rc(-6251)
    end,[27436]=function()
        o[1]=bb(sd(Na))
        ie=14953
    end,[14953]=function()
        o[2]='number'
        ie=25007
    end}
    ie=rc(7966)
    repeat
        while true do
            o[3]=Jc[ie]
            if o[3]~=nil then
                if o[3]()then
                    break
                end
            elseif ie==-31743 then
                return o[1]
            end
        end
    until ie==11186
end,function(la)
    local P,fb,ud,Ba,Bb
    ud={}
    Bb,P={[13465]=29222,[-4463]=24268,[-19387]=25186,[-15045]=25186,[31502]=24268},function(Rc)
        return Bb[Rc- -146]
    end
    Ba={[25186]=function()
        ud[1]=bb(sd(la))
        fb=-5353
    end,[-19498]=function()
        ud[1]=se(ud[1][1],1,ud[1][2])==ud[2]
        fb=P(-4609)
    end,[-5353]=function()
        ud[2]='string'
        fb=-19498
    end}
    fb=P(-15191)
    repeat
        while true do
            ud[3]=Ba[fb]
            if ud[3]~=nil then
                if ud[3]()then
                    break
                end
            elseif fb==24268 then
                return ud[1]
            end
        end
    until fb==-18095
end,function(l)
    local fe,Kb,Ic,ib,t
    Ic={}
    t,ib={[-29191]=-16195,[20200]=-14495,[18152]=2287,[16167]=-16195,[23462]=6070,[-15460]=23946,[18724]=-14495},function(Hb)
        return t[Hb- -32500]
    end
    Kb={[6070]=function()
        Ic[1]=se(Ic[1][1],1,Ic[1][2])==Ic[2]
        fe=ib(-61691)
    end,[2287]=function()
        Ic[2]='boolean'
        fe=ib(-9038)
    end,[-14495]=function()
        Ic[1]=bb(sd(l))
        fe=ib(-14348)
    end}
    fe=ib(-13776)
    repeat
        while true do
            Ic[3]=Kb[fe]
            if Ic[3]~=nil then
                if Ic[3]()then
                    break
                end
            elseif fe==-16195 then
                return Ic[1]
            end
        end
    until fe==8860
end,function(T)
    local Bd,ia,jd,L,da
    ia={}
    jd,da={[-22204]=-18656,[32262]=-13256,[31705]=-17240,[25202]=4963,[13472]=-13256,[478]=-1657,[-28510]=-17240},function(Td)
        return jd[Td+32766]
    end
    Bd={[-18656]=function()
        ia[1]=se(ia[1][1],1,ia[1][2])==ia[2]
        L=da(-504)
    end,[-17240]=function()
        ia[1]=bb(sd(T))
        L=-4139
    end,[-4139]=function()
        ia[2]='function'
        L=da(-54970)
    end}
    L=da(-1061)
    repeat
        while true do
            ia[3]=Bd[L]
            if ia[3]~=nil then
                if ia[3]()then
                    break
                end
            elseif L==-13256 then
                return ia[1]
            end
        end
    until L==-15103
end,{{5,2,true},{3,7,false},{2,7,false},{2,7,false},{0,5,true},{9,7,false},{0,8,false},{3,7,false},{3,3,true},{10,7,true},{3,5,true},{10,1,false},{10,7,true},{5,3,true},{5,3,false},{3,3,true},{10,4,true},{4,7,false},{3,2,true},{4,3,true},{3,3,false},{5,8,true},{0,2,true},{10,9,false},{3,4,true},{9,7,false},{10,3,true},{4,9,true},{1,3,false},{9,4,false},{0,0,false},{5,9,true},{4,4,false},{0,0,false},{10,0,true},{4,4,false},{3,9,false},{10,2,true},{0,2,false},{3,7,true},{10,8,false},{1,2,true},{3,5,false},{4,4,true},{3,5,true},{5,1,true},{2,7,false},{4,1,true},{4,1,true},{3,1,true},{4,7,false},{1,9,false},{10,4,true},{2,7,true},{1,2,false},{3,7,false},{10,4,false},{3,7,false},{4,7,true},{7,8,false},{4,0,true},{4,4,false},{6,7,false},{4,7,false},{3,7,false},{9,9,true},{1,0,true},{1,3,false},{1,7,false},{10,5,true},{3,3,false},{10,1,false},{4,9,true},{10,9,false},{10,7,true},{10,8,true},{6,9,true},{0,4,false},{1,1,false},{2,8,false},{5,7,false},{8,7,false},{3,1,false},{7,7,false},{3,4,true},{1,1,false},{4,1,true},{5,0,true},{0,9,false},{4,3,false},{3,0,true},{4,7,false},{5,7,false},{0,7,false},{10,8,true},{1,4,false},{5,1,false},{4,9,true},{1,7,false},{3,7,false},{0,0,false},{10,2,false},{1,3,false},{3,3,false},{6,7,false},{2,7,false},{6,9,true},{3,7,true},{5,7,false},{7,7,false},{1,4,false},{6,7,false},{3,9,true},{5,2,true},{10,5,false},{4,9,true},{6,7,false},{3,0,true},{10,1,true},{5,7,false},{3,4,false},{10,7,false},{4,7,false},{5,3,true},{3,4,false},{1,5,true},{6,9,true},{5,5,true},{3,9,false},{3,7,true},{3,5,true},{1,7,false},{4,3,false},{1,4,true},{9,7,false},{8,7,false},{4,0,true},{3,3,false},{3,7,true},{10,9,false},{10,1,true},{6,9,true},{1,2,true},{0,2,true},{8,7,false},{2,8,false},{4,3,true},{9,8,false},{4,1,true},{4,7,false},{0,1,true},{4,1,false},{10,2,true},{0,5,false},{3,7,false},{5,8,false},{8,9,true},{1,6,false},{6,7,false},{5,7,false},{0,7,true},{3,7,false},{10,0,true},{10,7,true},{0,1,false},{10,7,true},{1,8,true},{0,3,false},{4,4,true},{0,5,false},{5,4,true},{7,9,true},{10,2,false},{1,2,true},{2,7,false},{5,8,true},{9,8,false},{0,8,true},{0,3,true},{5,8,false},{0,7,true},{0,8,false},{4,3,false},{7,7,false},{0,7,false},{1,2,true},{4,8,false},{5,9,false},{3,7,false},{7,7,false},{3,2,true},{10,4,false},{3,7,false},{7,8,false},{3,7,true},{1,9,true},{0,8,false},{4,5,true},{5,7,false},{1,8,true},{1,2,true},{1,7,false},{4,7,true},{10,4,true},{3,9,false},{6,7,true},{1,4,false},{3,7,false},{5,9,false},{0,1,false},{9,8,false},{1,5,false},{4,7,false},{4,8,false},{3,8,false},{0,3,true},{1,7,true},{2,7,false},{3,1,true},{10,7,true},{3,1,true},{7,9,true},{3,7,false},{0,8,true},{1,8,true},{5,1,true},{5,8,true},{10,2,true},{5,5,false},{5,4,true},{4,3,false},{1,1,true},{4,0,true},{1,8,true},{4,4,true},{4,5,true},{5,2,true},{8,7,false},{1,0,true},{5,5,true},{4,4,false},{10,2,false},{10,2,false},{4,9,true},{4,9,false},{3,7,false},{3,3,false},{3,7,false},{4,4,true},{4,3,false},{9,7,false},{3,8,true},{7,7,false},{7,7,false},{10,9,false},{3,7,true}},-1,-2,{[4049]={},[-24313]={}}
local function hc(O)
    local ta,F,X,S,ee
    S={}
    ee,X={[-27597]=26218,[14349]=31276,[-26852]=-20936,[-14249]=-309,[-26862]=18194,[-11862]=16077,[-22838]=18194,[-10826]=-3473,[25361]=18194,[31406]=28514},function(Wc)
        return ee[Wc+-22804]
    end
    ta={[-3473]=function()
        S[1]=65536
        F=X(-4793)
    end,[31276]=function()
        S[2]=32768
        F=X(-4048)
    end,[-4083]=function()
        S[3]=S[4]
        F=X(48165)
    end,[26218]=function()
        S[1]=O-S[1]
        F=25864
    end,[24284]=function()
        if S[3]then
            F=X(54210)
            return true
        end
        F=X(11978)
    end,[25864]=function()
        S[5]=S[1]
        F=X(10942)
    end,[28514]=function()
        S[4]=O
        F=-4083
    end,[-20936]=function()
        S[2]=O<S[2]
        F=-1136
    end,[16077]=function()
        S[3]=S[5]
        F=X(-4058)
    end,[-1136]=function()
        S[3]=S[2]
        F=24284
    end}
    F=X(37153)
    repeat
        while true do
            S[6]=ta[F]
            if S[6]~=nil then
                if S[6]()then
                    break
                end
            elseif F==18194 then
                return S[3]
            end
        end
    until F==28794
end
local function U(wd)
    local Ja,Gb,Ob,Mc,Cc
    Ja={}
    Ob,Gb={[12447]=26493,[-13701]=20230,[-28043]=20230,[13465]=-19360,[-11762]=20230,[-5627]=-19360,[-22454]=451,[-23225]=-19406,[8337]=-15687},function(Zd)
        return Ob[Zd- -29878]
    end
    Mc={[26493]=function()
        Ja[1]=16777216
        Cc=24763
    end,[-11761]=function()
        if Ja[2]then
            Cc=Gb(-53103)
            return true
        end
        Cc=Gb(-17431)
    end,[21084]=function()
        Ja[2]=Ja[3]
        Cc=-11761
    end,[-19360]=function()
        Ja[3]=8388608
        Cc=Gb(-52332)
    end,[28547]=function()
        Ja[2]=Ja[4]
        Cc=Gb(-57921)
    end,[451]=function()
        Ja[3]=wd<Ja[3]
        Cc=21084
    end,[7884]=function()
        Ja[5]=Ja[1]
        Cc=-10995
    end,[-19406]=function()
        Ja[4]=wd
        Cc=28547
    end,[24763]=function()
        Ja[1]=wd-Ja[1]
        Cc=7884
    end,[-10995]=function()
        Ja[2]=Ja[5]
        Cc=Gb(-43579)
    end}
    Cc=Gb(-35505)
    repeat
        while true do
            Ja[6]=Mc[Cc]
            if Ja[6]~=nil then
                if Ja[6]()then
                    break
                end
            elseif Cc==20230 then
                return Ja[2]
            end
        end
    until Cc==-3841
end
local function Od(Fb)
    local Pa=Gc[-24313][Fb]
    if Pa then
        return Pa
    end
    local Lb,h=Fb,1
    local function ic()
        local Ca=tc('B',Lb,h)
        h=h+1
        return aa(Ca,178)
    end
    local function md()
        local Kd=tc('<I4',Lb,h)
        h=h+4
        return aa(Kd,-383298948)
    end
    local function Fd()
        local xd=tc('<d',Lb,h)
        h=h+8
        return xd
    end
    local function Ya(Ha)
        local kd=tc('c'..Ha,Lb,h)
        h=h+Ha
        return kd
    end
    local function _()
        local dc,Pd,Kc,Eb,sb
        dc={}
        Pd,Kc={[31109]=26735,[19883]=-16602,[-2239]=-26126,[23959]=25607,[20374]=-1463,[6124]=-26126,[-7732]=19702,[-6506]=-4000,[-26682]=-14514,[17934]=-24317,[-24218]=-7478,[-14423]=25047,[28340]=25607,[-5478]=-32418,[-30727]=26735,[16034]=1565,[-31926]=-27652,[1006]=23751,[-5840]=10706,[2578]=25607,[10748]=-27652,[-31502]=-4371,[4830]=-24317,[4006]=-16822,[-23753]=25607,[-4542]=32507,[28864]=-16822,[-9295]=-9284,[16484]=-9284,[8595]=1565,[-15875]=-7478,[-24713]=25607,[-24755]=-7478,[12664]=20035},function(ra)
            return Pd[ra-5612]
        end
        sb={[26735]=function()
            dc[1]=bb(ic())
            Eb=Kc(22096)
        end,[-28689]=function()
            dc[2]=dc[3]-dc[2]
            Eb=Kc(-2120)
        end,[27108]=function()
            dc[1]=127
            Eb=-5560
        end,[19702]=function()
            dc[4]=7
            Eb=Kc(18276)
        end,[-1463]=function()
            dc[1]=bb(aa(dc[5],dc[1]))
            Eb=Kc(14207)
        end,[-5560]=function()
            dc[1]=ha(dc[6],dc[1])
            Eb=-9299
        end,[-27652]=function()
            dc[1]=0
            Eb=Kc(9618)
        end,[-4000]=function()
            if(dc[7]>=0 and dc[8]>dc[9])or((dc[7]<0 or dc[7]~=dc[7])and dc[8]<dc[9])then
                Eb=Kc(-18141)
            else
                Eb=Kc(36721)
            end
        end,[-16602]=function()
            dc[1]=bb(Sb(dc[5],se(dc[1][1],1,dc[1][2])))
            Eb=Kc(11736)
        end,[10706]=function()
            Eb=Kc(8190)
            return true
        end,[-26126]=function()
            dc[5]=se(dc[1][1],1,dc[1][2]);
            if not _c(dc[6],128)then
                Eb=Kc(-228)
                return true
            end
            Eb=Kc(-19143)
        end,[-9299]=function()
            dc[2]=21
            Eb=-28689
        end,[-7478]=function()
            dc[8]=dc[8]+dc[7];
            dc[3]=dc[8];
            if dc[8]~=dc[8]then
                Eb=Kc(-19101)
            else
                Eb=-4000
            end
        end,[25607]=function()
            dc[1]=1773850256
            Eb=Kc(25986)
        end,[20035]=function()
            dc[2]=(dc[2])*dc[4]
            Eb=Kc(-8811)
        end,[31683]=function()
            dc[3]=dc[8];
            if dc[9]~=dc[9]then
                Eb=Kc(29571)
            else
                Eb=Kc(-894)
            end
        end,[-9284]=function()
            dc[6]=se(dc[1][1],1,dc[1][2]);
            Eb=27108;
        end,[25047]=function()
            dc[1]=bb(ja(dc[1],dc[2]))
            Eb=Kc(25495)
        end}
        Eb=Kc(-26314)
        repeat
            while true do
                dc[10]=sb[Eb]
                if dc[10]~=nil then
                    if dc[10]()then
                        break
                    end
                elseif Eb==1565 then
                    return se(dc[1][1],1,dc[1][2])
                elseif Eb==-16822 then
                    dc[5]=dc[1];
                    dc[8],dc[9],dc[7]=21,(4)+21,1
                    Eb=31683
                end
            end
        until Eb==23016
    end
    local function Vd()
        local Sc,ce,_e,bc,Ee
        Sc={}
        bc,_e={[-7756]=-30969,[31071]=-15428,[4500]=-27083,[19866]=-21447,[12509]=-25548,[-17626]=21560,[6814]=-22088,[-11726]=12767,[-3537]=-15603,[5802]=-22088,[19056]=21560,[-19497]=-15428,[15031]=-15428,[-24127]=-1198,[-9997]=-30969,[-1944]=-15603,[-1468]=12767,[30683]=12294,[-26696]=-14343},function(id)
            return bc[id+-1069]
        end
        ce={[-22088]=function()
            Sc[1]=bb(Ya(Sc[2]))
            Ee=_e(-8928)
        end,[-15428]=function()
            Ee=_e(13578);
            return true;
        end,[-14343]=function()
            Sc[1]=bb(_())
            Ee=_e(-2468)
        end,[12767]=function()
            Sc[1]=''
            Ee=_e(-16557)
        end}
        Ee=_e(-25627)
        repeat
            while true do
                Sc[3]=ce[Ee]
                if Sc[3]~=nil then
                    if Sc[3]()then
                        break
                    end
                elseif Ee==21560 then
                    return Sc[1]
                elseif Ee==-15603 then
                    Sc[2]=se(Sc[1][1],1,Sc[1][2]);
                    if Sc[2]==0 then
                        Ee=_e(-10657)
                        break
                    else
                        Ee=_e(6871)
                        break
                    end
                    Ee=_e(-18428)
                elseif Ee==-30969 then
                    return se(Sc[1][1],1,Sc[1][2])
                end
            end
        until Ee==-25548
    end
    local function hb(Zc)
        local Qd,wc,gb,Ia,Ge
        Ia={}
        gb,wc={[-3639]=21453,[7163]=-20524,[-30361]=-26820,[4393]=3558,[24239]=-18036,[-25411]=18960,[7412]=23474,[-6180]=-276,[-12184]=3558,[-28004]=-30199,[-4122]=-20440,[25275]=-24860,[-31984]=23474,[-4620]=7978,[-3073]=-10728,[26621]=27630,[27228]=9746,[22275]=-865,[14576]=239,[-7123]=-20440,[27704]=16731,[-15239]=22061,[1383]=23474,[-15912]=-17093,[9120]=2213,[15716]=-11585,[31212]=27408,[-1846]=10562,[-32728]=-7276,[31865]=-21366,[-32455]=-28652,[921]=2283,[23499]=23880,[5607]=-15947,[-8148]=21497,[24046]=-19937,[-24048]=26992,[-8596]=-21696,[-10855]=-28095,[11014]=-21558,[-3855]=-26820,[-9561]=5250,[20375]=-27424,[17557]=4839,[-20633]=-27821,[-3078]=21514,[-9616]=-15432,[-28009]=-13756,[-7775]=10562,[29346]=20943,[17219]=-18916,[-13389]=-2584,[-10701]=-20524,[30811]=4758,[2825]=-13756,[-27622]=3061,[-3886]=7978,[-28234]=-2584,[5549]=22061,[-28279]=-18916,[19744]=24512,[4760]=31471,[252]=-29077,[21558]=14870,[-8752]=9808,[31390]=-13756,[-20589]=-4757,[-1603]=-17093,[-6515]=-18556,[24385]=23474,[26090]=-11585,[5916]=-24588,[30229]=15693,[-19756]=25911,[-29390]=-16505,[-31233]=-19581,[-22895]=-3384,[-16870]=26026,[14412]=5899,[-12996]=21514,[-29624]=-18556,[2592]=23474,[9359]=-2172,[24842]=23474,[3017]=-5062,[-14613]=-2771,[-28462]=15553,[-3253]=7417,[7040]=-1673,[7261]=-15947,[13698]=20086,[-1635]=23880,[-29844]=29136,[17079]=11442,[2002]=8938,[32740]=-18248,[-23795]=-15783,[-5609]=16561,[-29625]=20086,[7230]=10311,[-30505]=16561,[-31120]=16399,[6307]=-4168,[-12569]=-31353,[-14107]=6118,[8204]=399,[10126]=19120,[-7348]=-20224,[32028]=15553,[19738]=14813,[15179]=-27545,[2842]=240,[7097]=19593,[28150]=9549,[-11569]=-21696,[5141]=-276,[8348]=26992,[-2908]=-15947,[4253]=-3489,[-2560]=23474,[-18194]=-31353,[-19934]=5899,[30848]=-28652,[19611]=449,[3475]=23474,[25247]=23474,[14594]=-3489,[29551]=5561,[-4536]=-23829,[25553]=27630,[-2915]=2420,[-5855]=11175,[-5129]=23474,[-31956]=-10089,[-18299]=2719,[24324]=-15947,[-8464]=239,[-24847]=9549,[-15205]=1074,[3065]=1074,[-4465]=-32709,[-10628]=-17889,[-6835]=5561,[8053]=-10728,[-30102]=-19581,[-25911]=6020,[-11372]=-30210,[-21406]=21453,[21345]=-18036,[27619]=-32537,[-11947]=30646,[-10389]=23474,[14731]=18960,[20097]=24512,[17946]=5048,[-2861]=31201,[-11848]=-18916,[5797]=6118,[3719]=-4757,[15089]=-27601,[-1330]=3976,[30917]=31194,[-2835]=23474,[10168]=-15947,[24332]=7417,[9377]=23474,[-7034]=31471,[15168]=-15947,[23284]=-22325,[24941]=5414,[1917]=9031},function(gd)
            return gb[gd-4007]
        end
        Ge={[18960]=function()
            Ia[1]=8
            Qd=18491
        end,[-21696]=function()
            Ia[2][29290]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(1146);
        end,[-15245]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(-7562)
        end,[-32709]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(32157)
        end,[11175]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(24104)
        end,[399]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(-17399)
        end,[9031]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=-8421
        end,[3976]=function()
            Ia[1]=255
            Qd=17206
        end,[-24860]=function()
            Ia[1]=16
            Qd=wc(-23615)
        end,[8938]=function()
            Ia[1]=16
            Qd=-16265
        end,[3061]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=-4600
        end,[21628]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=wc(-5609)
        end,[18328]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=wc(11104)
        end,[-8866]=function()
            Ia[3]=16777215
            Qd=wc(-1848)
        end,[-23829]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(25352)
        end,[-4757]=function()
            Ia[2][-22190]=se(Ia[1][1],1,Ia[1][2]);
            Qd=7893;
        end,[4758]=function()
            Ia[1]=Ia[5][Ia[1]]
            Qd=wc(12355)
        end,[-30199]=function()
            Ia[3]=255
            Qd=418
        end,[-2504]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=5085
        end,[15551]=function()
            Ia[1]=bb(hc(Ia[6]))
            Qd=wc(-28448)
        end,[-19524]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=wc(15021)
        end,[-27424]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(-16582)
        end,[30868]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(-8177)
        end,[31201]=function()
            Ia[1]=16
            Qd=wc(33353)
        end,[31196]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=wc(-23997)
        end,[9549]=function()
            Ia[2][-22190]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(28849)
        end,[9746]=function()
            if Ia[7]==8 then
                Qd=wc(10314)
                return true
            elseif Ia[7]==3 then
                Qd=wc(18601)
                return true
            end
            Qd=wc(1172)
        end,[-5062]=function()
            Ia[1]=kb(Zc,Ia[1])
            Qd=wc(19175)
        end,[-18104]=function()
            Ia[1]=kb(Zc,Ia[2])
            Qd=wc(-24227)
        end,[-16505]=function()
            Ia[3]=255
            Qd=wc(-6848)
        end,[20213]=function()
            Ia[1]=24
            Qd=wc(1092)
        end,[24512]=function()
            Ia[6]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(2372);
        end,[-1673]=function()
            Ia[3]=255
            Qd=wc(12211)
        end,[10562]=function()
            Ia[1]=8
            Qd=30480
        end,[-15783]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(-613)
        end,[31471]=function()
            Ia[8]=3
            Qd=wc(-27949)
        end,[2213]=function()
            Ia[1]=bb(U(Ia[6]))
            Qd=wc(754)
        end,[-4249]=function()
            Ia[1]=1
            Qd=wc(-21904)
        end,[2719]=function()
            Ia[1]={}
            Qd=wc(7024)
        end,[-17093]=function()
            Ia[1]=8
            Qd=wc(34924)
        end,[10311]=function()
            Ia[1]=16
            Qd=wc(5924)
        end,[-16265]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=12600
        end,[26992]=function()
            Ia[3]=2
            Qd=10686
        end,[7541]=function()
            Ia[3]=255
            Qd=-15245
        end,[-22238]=function()
            Ia[3]=255
            Qd=30868
        end,[-18556]=function()
            Ia[9]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(-10100);
        end,[418]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(-24455)
        end,[-28095]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(19723)
        end,[10686]=function()
            Ia[3]=Ia[5][Ia[3]]
            Qd=wc(8767)
        end,[6020]=function()
            Ia[1]=Ia[10]+Ia[1]
            Qd=wc(26282)
        end,[5899]=function()
            Ia[2][-22190]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(6599)
        end,[3558]=function()
            Ia[2][-22190]=se(Ia[1][1],1,Ia[1][2]);
            Qd=-22141;
        end,[-27601]=function()
            Ia[3]=255
            Qd=wc(-4141)
        end,[-27821]=function()
            Ia[1]=bb(md())
            Qd=wc(17705)
        end,[-21558]=function()
            Ia[3]=255
            Qd=wc(-458)
        end,[24402]=function()
            Ia[3]=255
            Qd=wc(4928)
        end,[22061]=function()
            Ia[2][-28909]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(-3341);
        end,[23880]=function()
            Ia[2][-32547]=Ia[6];
            Qd=wc(13127);
        end,[-11585]=function()
            Ia[2][-28909]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(29282);
        end,[7893]=function()
            Ia[1]=16
            Qd=21628
        end,[-29477]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(-15927)
        end,[27630]=function()
            Ia[2][-22190]=se(Ia[1][1],1,Ia[1][2]);
            Qd=32522;
        end,[-22141]=function()
            Ia[1]=16
            Qd=18328
        end,[-18036]=function()
            Ia[2][29290]=se(Ia[1][1],1,Ia[1][2]);
            Qd=-7517;
        end,[-20524]=function()
            Ia[2][15704]=Ia[6];
            Qd=15551;
        end,[17206]=function()
            Ia[1]=bb(ha(Ia[4],Ia[1]))
            Qd=wc(12060)
        end,[12600]=function()
            Ia[3]=255
            Qd=2221
        end,[20193]=function()
            Ia[1]=1
            Qd=wc(34818)
        end,[16561]=function()
            Ia[2][29290]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(13384)
        end,[16731]=function()
            if not(Ia[7]==6)then
                Qd=wc(36747)
                return true
            else
                Qd=wc(-27113)
                return true
            end
            Qd=wc(-1122)
        end,[4284]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=wc(11047)
        end,[-7517]=function()
            Ia[1]=24
            Qd=-2504
        end,[15938]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(-1602)
        end,[-4168]=function()
            Ia[1]=8
            Qd=wc(34236)
        end,[18491]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=7541
        end,[-15432]=function()
            Ia[3]=65535
            Qd=-14190
        end,[32522]=function()
            Ia[1]=24
            Qd=4284
        end,[2283]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(18583)
        end,[-26820]=function()
            Ia[6]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(-6694);
        end,[15693]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=wc(-25383)
        end,[-18248]=function()
            if not(Ia[7]==9)then
                Qd=wc(31235)
                return true
            else
                Qd=wc(-11905)
                return true
            end
            Qd=wc(11419)
        end,[1074]=function()
            Ia[2][29290]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(6009);
        end,[25911]=function()
            Ia[1]={[-30958]=0,[-10759]=0,[-18832]=0,[-32547]=0,[15917]=0,[6747]=nil,[6258]=Ia[11],[21753]=0,[-28909]=0,[-22190]=0,[4767]=0,[15704]=0,[31907]=0,[28498]=Ia[10],[29290]=0,[3800]=0}
            Qd=wc(33558)
        end,[239]=function()
            Ia[2][-28909]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(11237);
        end,[2221]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(9556)
        end,[21497]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(-3116)
        end,[4839]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=24835
        end,[-20597]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(30628)
        end,[-17889]=function()
            Ia[1]=8
            Qd=29
        end,[-20224]=function()
            Ia[1]=24
            Qd=-19524
        end,[-28652]=function()
            Ia[2][3800]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(28392)
        end,[-3489]=function()
            Ia[1]=8
            Qd=17182
        end,[29]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=-8866
        end,[-21366]=function()
            Ia[1]=bb(md())
            Qd=wc(-2508)
        end,[24835]=function()
            Ia[3]=255
            Qd=-13496
        end,[15553]=function()
            Ia[2][-28909]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(5390)
        end,[7978]=function()
            Ia[2][-28909]=se(Ia[1][1],1,Ia[1][2]);
            Qd=20213;
        end,[-4600]=function()
            Ia[3]=255
            Qd=-20597
        end,[2420]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=22473
        end,[21453]=function()
            Ia[2][29290]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(29254)
        end,[-10089]=function()
            Ia[8]=Ia[5][Ia[8]]
            Qd=wc(-14187)
        end,[5085]=function()
            Ia[3]=255
            Qd=-29477
        end,[22473]=function()
            Ia[3]=255
            Qd=15938
        end,[-8421]=function()
            Ia[3]=255
            Qd=wc(-529)
        end,[19593]=function()
            Ia[3]=255
            Qd=wc(-19788)
        end,[6118]=function()
            Ia[2][-18832]=Ia[9];
            Qd=wc(-14292);
        end,[11442]=function()
            Ia[3]=255
            Qd=wc(24382)
        end,[-14190]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(-26354)
        end,[-865]=function()
            Ia[1]=D[Ia[1]]
            Qd=wc(-27226)
        end,[31194]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=24402
        end,[17182]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=wc(21086)
        end,[20943]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=wc(19096)
        end,[16399]=function()
            Ia[1]=8
            Qd=wc(21564)
        end,[-13496]=function()
            Ia[1]=bb(ha(Ia[1],Ia[3]))
            Qd=wc(-11198)
        end,[-20440]=function()
            Ia[2][-22190]=se(Ia[1][1],1,Ia[1][2]);
            Qd=21354;
        end,[21354]=function()
            Ia[1]=24
            Qd=31196
        end,[7417]=function()
            Ia[2][-30958]=se(Ia[1][1],1,Ia[1][2]);
            Qd=wc(-27977)
        end,[30480]=function()
            Ia[1]=ue(Ia[4],Ia[1])
            Qd=-22238
        end}
        Qd=wc(-16626)
        repeat
            while true do
                Ia[12]=Ge[Qd]
                if Ia[12]~=nil then
                    if Ia[12]()then
                        break
                    end
                elseif Qd==20086 then
                    Ia[4]=se(Ia[1][1],1,Ia[1][2]);
                    Qd=wc(2677);
                elseif Qd==-19581 then
                    Ia[5]=Ia[1];
                    Qd=20193;
                elseif Qd==-15947 then
                    return Ia[13]
                elseif Qd==-10728 then
                    Ia[10]=se(Ia[1][1],1,Ia[1][2]);
                    Qd=-4249;
                elseif Qd==5561 then
                    Ia[2]=Ia[1];
                    Qd=-18104;
                elseif Qd==-31353 then
                    Ia[7],Ia[11],Ia[13]=Ia[1],Ia[3],Ia[8];
                    Qd=wc(-15749);
                elseif Qd==-2584 then
                    if Ia[7]==2 then
                        Qd=wc(-21404)
                        break
                    elseif Ia[7]==1 then
                        Qd=wc(-6621)
                        break
                    elseif not(Ia[7]==4)then
                        Qd=wc(31711)
                        break
                    else
                        Qd=wc(-3768)
                        break
                    end
                    Qd=23474
                elseif Qd==23474 then
                    if Ia[13]then
                        Qd=wc(35872)
                        break
                    end
                    Qd=wc(9614)
                end
            end
        until Qd==253
    end
    local function Xc(vb,xb)
        local g,Yb,Pc,K,A
        A={}
        K,Pc={[-22269]=28526,[26489]=5454,[19397]=1161,[28535]=-7478,[1331]=11714,[15240]=7720,[11384]=-29608,[1380]=-29065,[-11157]=-31216,[-264]=16996,[-32501]=-6405,[-7658]=-31421,[3415]=-23377,[5455]=16996,[9455]=8018,[-6713]=5820,[-4614]=11714,[-4377]=1730,[-16150]=5454,[10912]=1730,[13249]=-29608,[-4941]=-22636,[32199]=-20105,[-11520]=13470,[13099]=11036,[31650]=-30154,[-4826]=-12089,[19489]=4443,[-28189]=11714,[9342]=11426,[19880]=-11558,[10303]=-29744,[32597]=-2530,[-8092]=11714,[-31704]=-20031,[12929]=-26829,[-21478]=11714,[16494]=11714,[18135]=28526,[-3811]=11714,[-20161]=-10949,[25609]=17029,[-28677]=13258,[4007]=-472,[-15080]=-30154,[11199]=-31000,[-4537]=2115,[-10970]=11036,[4230]=25266,[-19982]=-472,[9438]=27311,[9845]=-16550,[-29138]=11714,[20513]=-6760,[1067]=-7478,[-9997]=-23377,[15215]=-31216,[-31862]=-20031,[-664]=-6760,[4398]=30051,[-10681]=13258,[1519]=-17877,[17373]=10966,[-23089]=-6760,[-5844]=11824,[-18155]=28492,[8613]=-17877,[-26942]=15393,[-6322]=11714,[12032]=-24755,[-6362]=4769,[-5120]=5820,[8051]=14496,[-4769]=-6760,[1311]=22077,[-25572]=-27223,[21391]=11714,[-32688]=12828,[-27867]=-6642,[-5865]=-8306,[-17487]=-20977,[-28674]=-30562,[26668]=-14950,[4466]=11714,[-18714]=-30562,[-32707]=-31000,[10113]=2007,[-17280]=-28110,[9466]=11714,[-8122]=-28110,[6186]=-15158,[11634]=11714,[9723]=11714,[1839]=-22636,[-15735]=-10546,[32612]=-24755,[-8026]=27311},function(z)
            return K[z+-6890]
        end
        Yb={[9740]=function()
            A[1]=ue(A[2],A[1])
            g=24286
        end,[-10949]=function()
            A[1]=1
            g=-14613
        end,[28526]=function()
            vb[-10759]=A[3];
            g=Pc(11356)
        end,[25266]=function()
            A[3]=vb[A[3]]
            g=Pc(24263)
        end,[-28110]=function()
            A[4]=A[3];
            if not(A[4]==9)then
                g=Pc(1025)
                return true
            else
                g=Pc(32499)
                return true
            end
            g=Pc(18524)
        end,[12828]=function()
            A[3]=xb[A[3]]
            g=Pc(-13092)
        end,[-20105]=function()
            A[3]=xb[A[3]]
            g=Pc(12345)
        end,[11714]=function()
            g=Pc(26287);
            return true;
        end,[-22636]=function()
            A[3]=-30958
            g=Pc(11120)
        end,[-31216]=function()
            A[5],A[6]=A[3],se(A[1][1],1,A[1][2]);
            g=2132;
        end,[-20031]=function()
            A[7],A[8]=A[3],se(A[1][1],1,A[1][2]);
            g=Pc(-11265);
        end,[-31421]=function()
            A[3]=A[7]+A[3]
            g=12974
        end,[-31000]=function()
            vb[21753]=A[5];
            if A[5]==2 then
                g=Pc(177)
                return true
            elseif A[5]==3 then
                g=Pc(-20977)
                return true
            end
            g=Pc(28281)
        end,[1730]=function()
            vb[31907]=A[3];
            g=Pc(568)
        end,[12974]=function()
            A[3]=xb[A[3]]
            g=Pc(35425)
        end,[-14129]=function()
            A[1]=ue(A[2],A[1])
            g=15468
        end,[-1860]=function()
            A[3]=A[8]+A[3]
            g=Pc(19819)
        end,[-8306]=function()
            if A[4]==5 then
                g=Pc(15503)
                return true
            elseif not(A[4]==3)then
                g=Pc(17003)
                return true
            else
                g=Pc(528)
                return true
            end
            g=Pc(2276)
        end,[5454]=function()
            A[3]=6258
            g=Pc(2064)
        end,[4769]=function()
            A[3]=3800
            g=-17166
        end,[-12089]=function()
            A[3]=vb[A[3]]
            g=Pc(-1232)
        end,[-30562]=function()
            A[2]=A[3];
            g=Pc(16232);
        end,[2115]=function()
            A[1]=1023
            g=32053
        end,[-6642]=function()
            A[3]=10
            g=Pc(8270)
        end,[-18989]=function()
            A[1]=bb(ha(A[1],A[9]))
            g=Pc(22105)
        end,[-3886]=function()
            A[3]=ue(A[2],A[3])
            g=Pc(8201)
        end,[-14950]=function()
            A[1]=1
            g=Pc(16735)
        end,[5348]=function()
            A[3]=bb(ha(A[3],A[1]))
            g=Pc(38540)
        end,[9673]=function()
            A[1]=bb(ha(A[1],A[9]))
            g=Pc(-24972)
        end,[-30154]=function()
            A[7]=se(A[3][1],1,A[3][2]);
            g=-22600;
        end,[-22600]=function()
            A[3]=1
            g=-6678
        end,[5820]=function()
            A[3]=10
            g=-3886
        end,[-6678]=function()
            A[3]=A[7]+A[3]
            g=-1365
        end,[17029]=function()
            A[3]=-18832
            g=-30999
        end,[-1365]=function()
            A[3]=xb[A[3]]
            g=Pc(20139)
        end,[15393]=function()
            A[3]=A[6]+A[3]
            g=Pc(-25798)
        end,[15468]=function()
            A[9]=1023
            g=-18989
        end,[27311]=function()
            vb[-10759]=A[3];
            g=Pc(16613)
        end,[32053]=function()
            A[3]=ha(A[3],A[1])
            g=Pc(-3107)
        end,[-26829]=function()
            A[3]=xb[A[3]]
            g=Pc(17802)
        end,[-14613]=function()
            A[3]=A[3]+A[1]
            g=Pc(39089)
        end,[11036]=function()
            A[1]=20
            g=-14129
        end,[-17877]=function()
            A[3]=-18832
            g=Pc(-4630)
        end,[11415]=function()
            A[3]=A[3]+A[1]
            g=-21700
        end,[2132]=function()
            A[3]=1
            g=Pc(-20052)
        end,[10966]=function()
            A[1]=1
            g=11415
        end,[-10546]=function()
            A[3]=xb[A[3]]
            g=Pc(-15379)
        end,[-17166]=function()
            A[3]=vb[A[3]]
            g=Pc(33558)
        end,[16996]=function()
            vb[-10759]=A[3];
            g=Pc(3079)
        end,[11426]=function()
            A[3]=30
            g=5008
        end,[22077]=function()
            A[1]=1023
            g=5348
        end,[-472]=function()
            vb[-10759]=A[3];
            g=Pc(-25817);
        end,[25042]=function()
            A[3]=1
            g=-1860
        end,[-23377]=function()
            A[1]=0
            g=9740
        end,[-16550]=function()
            A[3]=A[3]+A[1]
            g=Pc(-8845)
        end,[5008]=function()
            A[3]=ue(A[2],A[3])
            g=Pc(19989)
        end,[-29608]=function()
            vb[15917]=A[3];
            g=Pc(-22248)
        end,[-29065]=function()
            A[3]=ue(A[2],A[3])
            g=Pc(2353)
        end,[-21700]=function()
            A[3]=xb[A[3]]
            g=Pc(-1136)
        end,[13470]=function()
            A[3]=vb[A[3]]
            g=Pc(-11824)
        end,[-30999]=function()
            A[3]=vb[A[3]]
            g=Pc(-13271)
        end,[28492]=function()
            A[3]=1
            g=Pc(-768)
        end,[-7478]=function()
            vb[15917]=A[3];
            g=25042;
        end,[24286]=function()
            A[9]=1023
            g=9673
        end,[2007]=function()
            if not(A[4]==6)then
                g=Pc(-1202)
                return true
            else
                g=Pc(8729)
                return true
            end
            g=Pc(23384)
        end}
        g=Pc(33379)
        repeat
            while true do
                A[10]=Yb[g]
                if A[10]~=nil then
                    if A[10]()then
                        break
                    end
                end
            end
        until g==1161
    end
    local function c()
        local qe,f,k,ob,mc
        mc={}
        ob,qe={[-18320]=20724,[-28662]=6623,[6232]=-2765,[32631]=21135,[23516]=18340,[-2966]=31139,[27110]=20724,[7002]=-7061,[-20400]=25935,[-22998]=-20818,[-26901]=-21575,[-236]=21135,[-3172]=-16598,[-25055]=10362,[-21825]=32142,[-14523]=19127,[-3204]=-16598,[14767]=22604,[13856]=-21575,[8047]=-9858,[-31552]=4871,[26789]=-21025,[23018]=15680,[4282]=22920,[-29571]=22417,[29825]=24990,[28287]=23147,[16593]=-27408,[22316]=22604,[11648]=30021,[25457]=8004,[-30241]=-22275,[11924]=178,[-11497]=27955,[15120]=-7061,[-20256]=-2765,[31167]=10362,[12165]=-27609,[-32766]=3609,[-10805]=-10052,[-6883]=19689,[-3727]=-16598,[-21357]=-5591,[-22710]=-28099,[-11058]=27756,[-4589]=15061,[-28321]=-9874,[-27497]=-10052,[-2731]=28602,[-4073]=8286,[-29931]=-552,[2835]=-25784,[19005]=8286,[3829]=-11282,[13153]=11749,[7484]=4871,[15500]=-14096,[-25883]=20624,[1405]=13967,[28092]=-22092,[-2117]=30017,[-22047]=6623,[21773]=6623,[-22206]=15061,[-28127]=-15765,[-2552]=30017,[-11596]=-14003,[-387]=-20818,[32256]=15680,[-19247]=8004,[14377]=-16598,[-29819]=-21025,[6121]=8667,[20320]=-15765,[12466]=8004,[-23047]=17730,[-14021]=-9874,[-11622]=16774,[18898]=-13564,[-9004]=10362,[-8665]=8286,[12961]=-10052,[-21667]=3609,[17275]=8667,[-1523]=20487,[-3569]=-10052,[7052]=-5230,[-22462]=6623,[15370]=18340,[130]=20487,[27215]=4871,[4382]=178},function(i)
            return ob[i+7480]
        end
        k={[6623]=function()
            mc[1]=mc[1]+mc[2];
            mc[3]=mc[1];
            if mc[1]~=mc[1]then
                f=qe(-11049)
            else
                f=qe(-18538)
            end
        end,[10362]=function()
            mc[4]=bb(_())
            f=qe(7287)
        end,[8286]=function()
            mc[5]=mc[5]+mc[6];
            mc[3]=mc[5];
            if mc[5]~=mc[5]then
                f=qe(23687)
            else
                f=15809
            end
        end,[178]=function()
            mc[4]=bb(mc[7]())
            f=qe(19630)
        end,[15809]=function()
            if(mc[6]>=0 and mc[5]>mc[8])or((mc[6]<0 or mc[6]~=mc[6])and mc[5]<mc[8])then
                f=qe(-32535)
            else
                f=qe(-19102)
            end
        end,[13967]=function()
            if mc[9]then
                f=qe(-39032)
                return true
            else
                f=qe(-478)
                return true
            end
            f=qe(-29527)
        end,[16564]=function()
            mc[10]=566
            f=qe(4168)
        end,[-5230]=function()
            mc[3]=mc[5];
            if mc[8]~=mc[8]then
                f=qe(-16484)
            else
                f=15809
            end
        end,[16774]=function()
            mc[4]=126
            f=qe(22345)
        end,[17730]=function()
            mc[4]=bb(Qc(mc[11]))
            f=qe(-7350)
        end,[-16598]=function()
            mc[4]=-13904
            f=qe(20612)
        end,[-2765]=function()
            mc[12][(mc[3]-35)]=se(mc[4][1],1,mc[4][2]);
            f=qe(-10446)
        end,[3609]=function()
            mc[9]=se(mc[4][1],1,mc[4][2]);
            f=qe(-36142)
        end,[-31245]=function()
            if(mc[13]>=0 and mc[14]>mc[15])or((mc[13]<0 or mc[13]~=mc[13])and mc[14]<mc[15])then
                f=11749
            else
                f=qe(-3098)
            end
        end,[22920]=function()
            mc[4]=bb(c())
            f=qe(-1248)
        end,[-15765]=function()
            mc[10]=ic()
            f=qe(-7867)
        end,[-21392]=function()
            mc[16]=23720
            f=8255
        end,[-22092]=function()
            mc[17]=24692
            f=16564
        end,[-14003]=function()
            mc[4]=Qc(mc[18])
            f=qe(17977)
        end,[-21575]=function()
            mc[4]=ic()
            f=qe(-9597)
        end,[-7061]=function()
            mc[4]=bb(hb(mc[19]))
            f=qe(-40246)
        end,[30017]=function()
            mc[17]=ic()
            f=qe(-35607)
        end,[25935]=function()
            mc[14]=mc[14]+mc[13];
            mc[3]=mc[14];
            if mc[14]~=mc[14]then
                f=qe(5673)
            else
                f=-31245
            end
        end,[20724]=function()
            mc[20][(mc[3]-36)]=se(mc[4][1],1,mc[4][2]);
            f=qe(-27880)
        end,[-27082]=function()
            mc[4]=Xc(mc[4],mc[20])
            f=qe(-11553)
        end,[-27609]=function()
            if(mc[21]>=0 and mc[22]>mc[23])or((mc[21]<0 or mc[21]~=mc[21])and mc[22]<mc[23])then
                f=qe(-10652)
            else
                f=qe(-3198)
            end
        end,[31139]=function()
            mc[22]=mc[22]+mc[21];
            mc[3]=mc[22];
            if mc[22]~=mc[22]then
                f=qe(-11207)
            else
                f=qe(4685)
            end
        end,[-552]=function()
            mc[3]=mc[1];
            if mc[24]~=mc[24]then
                f=qe(5481)
            else
                f=27756
            end
        end,[-2735]=function()
            mc[4]=mc[19][(mc[4])]
            f=-27082
        end,[27756]=function()
            if(mc[2]>=0 and mc[1]>mc[24])or((mc[2]<0 or mc[2]~=mc[2])and mc[1]<mc[24])then
                f=qe(-34977)
            else
                f=qe(-6075)
            end
        end,[24990]=function()
            mc[4]=mc[3]-mc[4]
            f=-2735
        end,[-25784]=function()
            mc[3]=mc[14];
            if mc[15]~=mc[15]then
                f=11749
            else
                f=-31245
            end
        end,[8255]=function()
            mc[4]={[mc[4]]=mc[19],[mc[17]]=mc[25],[mc[10]]=mc[26],[mc[27]]=mc[12],[mc[28]]=mc[29],[mc[16]]=mc[30]}
            f=qe(16036)
        end,[-10052]=function()
            mc[4]=bb(_())
            f=qe(-35801)
        end,[23147]=function()
            mc[3]=mc[22];
            if mc[23]~=mc[23]then
                f=qe(6897)
            else
                f=-27609
            end
        end,[-22275]=function()
            mc[28]=22867
            f=-21392
        end,[-20818]=function()
            mc[27]=''
            f=qe(9795)
        end,[8667]=function()
            mc[28]=bb(_())
            f=qe(-7716)
        end,[30021]=function()
            mc[27]=4460
            f=qe(-37721)
        end,[4871]=function()
            mc[9]=false;
            f=qe(14293)
        end,[11088]=function()
            mc[4]=bb(Qc(mc[31]))
            f=qe(-12069)
        end}
        f=qe(-34381)
        repeat
            while true do
                mc[32]=k[f]
                if mc[32]~=nil then
                    if mc[32]()then
                        break
                    end
                elseif f==20487 then
                    mc[20]=se(mc[4][1],1,mc[4][2]);
                    mc[7]=function()
                        local Xd,J,Ae,ka,Xa
                        Xa={}
                        J,Ae={[-28930]=3435,[30031]=-5209,[12188]=31951,[-9751]=-5585,[-26087]=-32503,[-21146]=-5209,[14604]=-23610,[16317]=-22337,[11]=-32503,[22288]=-11025,[15849]=-5209,[-6945]=-11025,[-30047]=-5209,[-17873]=-22337,[-7761]=31951,[21148]=-22337,[-31059]=-5209,[21627]=-796,[13862]=11053,[4836]=-5209,[8970]=10528,[12956]=-5907,[-2587]=-23610,[7165]=-23610,[-18447]=-28002,[24363]=1103,[22686]=-5907,[-26358]=-22337,[-8031]=-8755,[21818]=-28002,[12211]=-25409},function(de)
                            return J[de- -28927]
                        end
                        Xd={[11053]=function()
                            Xa[1]=bb(_())
                            ka=Ae(-35872)
                        end,[-11025]=function()
                            Xa[2]=se(Xa[1][1],1,Xa[1][2]);
                            ka=Ae(1104)
                        end,[-28002]=function()
                            Xa[1]=bb(Vd())
                            ka=Ae(-36688)
                        end,[-32503]=function()
                            Xa[1]=bb(ic())
                            ka=Ae(-15971)
                        end,[31951]=function()
                            Xa[2]=se(Xa[1][1],1,Xa[1][2]);
                            ka=Ae(-13078)
                        end,[-23610]=function()
                            Xa[2]=nil;
                            ka=Ae(-58974)
                        end}
                        ka=Ae(-28916)
                        repeat
                            while true do
                                Xa[3]=Xd[ka]
                                if Xa[3]~=nil then
                                    if Xa[3]()then
                                        break
                                    end
                                elseif ka==-5209 then
                                    return Xa[2]
                                elseif ka==-5907 then
                                    Xa[4]=se(Xa[1][1],1,Xa[1][2]);
                                    if Xa[4]==1 then
                                        ka=Ae(-47374)
                                        break
                                    elseif Xa[4]==3 then
                                        ka=Ae(-15065)
                                        break
                                    elseif Xa[4]==2 then
                                        ka=Ae(-31514)
                                        break
                                    end
                                    ka=Ae(-24091)
                                end
                            end
                        until ka==-29718
                    end
                    mc[13],mc[14],mc[15]=1,37,(mc[11])+36
                    f=qe(-4645)
                elseif f==21135 then
                    mc[26],mc[30],mc[29],mc[25],mc[18]=mc[4],mc[17],mc[10],mc[27],se(mc[28][1],1,mc[28][2]);
                    f=qe(-19076);
                elseif f==18340 then
                    return mc[4]
                elseif f==11749 then
                    mc[5],mc[8],mc[6]=127,(mc[18])+126,1
                    f=qe(-428)
                elseif f==-9874 then
                    mc[11]=se(mc[4][1],1,mc[4][2]);
                    f=qe(-30527);
                elseif f==22604 then
                    mc[31]=se(mc[4][1],1,mc[4][2]);
                    f=11088;
                elseif f==15061 then
                    mc[12]=se(mc[4][1],1,mc[4][2]);
                    mc[21],mc[23],mc[22]=1,(mc[31])+35,36
                    f=qe(20807)
                elseif f==8004 then
                    mc[19],mc[9]=mc[4],false;
                    mc[1],mc[24],mc[2]=27,(mc[18])+26,1
                    f=qe(-37411)
                end
            end
        until f==-648
    end
    local zb=c()
    local ke,rd,Y
    rd,ke={[10787]=22597,[28268]=-18000,[-6401]=2598,[18124]=4787,[27362]=2598,[20634]=-21343,[-8460]=22597,[-800]=2598},function(ea)
        return rd[ea-32430]
    end
    Y=ke(31630)
    repeat
        while true do
            if Y==22597 then
                return zb
            elseif Y==2598 then
                Gc[-24313][Fb]=zb;
                Y=ke(23970);
            end
        end
    until Y==-31968
end
local xc=getfenv()
local function ge(M,od)
    M=Od(M)
    local H=M
    local function ze(Wd,te)
        local function Wb(...)
            return{[-32174]={...},[13017]=hd('#',...)}
        end
        local function Pb(td,Cb,Hc)
            local ld,a,cd,Aa,Ib
            Ib={}
            Aa,cd={[-1641]=-7265,[26827]=29224,[9376]=31433,[31887]=-3861,[17540]=-13296,[-31824]=18881,[28894]=-6128,[-18411]=-28320,[17866]=-23666,[-9247]=-3861,[15712]=16221,[7300]=31433,[-15582]=3836,[25279]=31433,[6173]=3836,[-5786]=-28320,[-5169]=-7265,[14462]=31433},function(Dc)
                return Aa[Dc-24827]
            end
            ld={[31433]=function()
                Ib[1]=td[Cb]
                a=cd(31000)
            end,[18881]=function()
                Ib[2]=Cb+Ib[2]
                a=cd(51654)
            end,[3836]=function()
                Ib[2]=1
                a=cd(-6997)
            end,[29224]=function()
                Ib[2]=bb(Pb(td,Ib[2],Hc))
                a=cd(19658)
            end}
            a=cd(42367)
            repeat
                while true do
                    Ib[3]=ld[a]
                    if Ib[3]~=nil then
                        if Ib[3]()then
                            break
                        end
                    elseif a==-7265 then
                        return Ib[1],se(Ib[2][1],1,Ib[2][2])
                    elseif a==-3861 then
                        return
                    elseif a==-13296 then
                        if not(Cb>Hc)then
                            a=cd(34203)
                            break
                        else
                            a=cd(15580)
                            break
                        end
                        a=cd(39289)
                    end
                end
            until a==2649
        end
        local function Ua(Ab,Qa,ad,_a)
            local ae,Q,jb,ba,Bc
            Q={}
            jb,ae={[-17805]=-8611,[18631]=14614,[2356]=16665,[11563]=-10309,[8980]=-12951,[2453]=5596,[26317]=20043,[20997]=-5631,[25933]=2109,[11003]=13019,[-14118]=-19601,[-12075]=-26199,[22450]=14767,[20106]=14614,[1756]=11365,[-15301]=-8749,[9092]=5685,[22550]=29023,[-30839]=-9936,[-13997]=29647,[-29979]=26118,[23366]=18273,[1441]=11984,[-17224]=-5583,[31790]=-4381,[6399]=14614,[21102]=-7758,[-3815]=30900,[9350]=-17112,[25965]=14614,[-22433]=12410,[-2187]=-21971,[-28676]=-17187,[-868]=8107,[-12706]=32670,[-16789]=-14958,[12180]=10594,[-27825]=12591,[-14392]=14780,[-14210]=-22117,[10862]=10709,[-2091]=-8861,[-18126]=29986,[15778]=13456,[5020]=14614,[-12804]=11976,[5238]=14614,[-335]=342,[-8200]=-2865,[18968]=9553,[-29378]=-8617,[24029]=-31003,[-22185]=7936,[-10630]=22601,[-27005]=-29991,[-31624]=-8064,[-14635]=-1568,[12439]=4060,[-12822]=2084,[-14401]=-18849,[22243]=8871,[-25985]=14614,[28276]=13518,[-8880]=11984,[-25041]=20043,[-29708]=7932,[-17735]=-17276,[-31742]=-1799,[-16120]=-13534,[12144]=14614,[17979]=28543,[-31331]=-31288,[-9419]=-14244,[-14885]=-32202,[-1386]=14614,[-20301]=10152,[-22313]=-13532,[9392]=30359,[30913]=-21645,[9050]=26073,[-4659]=-31288,[10833]=-4836,[-16263]=8871,[-22879]=15960,[3607]=14614,[-14493]=14614,[23525]=14614,[-26724]=23455,[9065]=9445,[-15009]=14614,[6090]=-4120,[-3627]=12790,[-29991]=-16260,[1440]=2372,[-28819]=14614,[9688]=27831,[-10181]=7936,[17909]=-24945,[-11159]=9110,[15127]=-6999,[-13905]=-4836,[29469]=-520,[-17691]=16723,[-16478]=29546,[14809]=8034,[-10222]=-16441,[-13767]=26959,[-23612]=-2836,[10946]=26674,[-4736]=24647,[3582]=17203,[-551]=-6145,[-5209]=19727,[25270]=-12951,[-534]=-28648,[11233]=-15821,[2776]=9573,[-26092]=-30845,[20282]=-5583,[28855]=17002,[-31716]=693,[9060]=29274,[16426]=-25256,[6729]=-8642,[31836]=-10309,[23397]=-28733,[-4775]=15960,[-5185]=9017,[25467]=26990,[1258]=-11881,[3402]=-19849,[-23797]=2982,[25184]=-12959,[20824]=7474,[-9952]=5684,[8477]=-8557,[10183]=14614,[7049]=-14244,[-14125]=-28331,[2987]=20941,[-26387]=-10600,[-2878]=-15815,[-6018]=-25060,[-26835]=11135,[-17221]=-8423,[-31725]=-14568,[5440]=-32723,[-23960]=-5220,[15213]=-11141,[-754]=-4294,[-4356]=20960,[-10394]=29446,[18854]=-15821,[-27576]=-28358,[15129]=31990,[-8287]=21579,[30858]=15960,[6871]=-1799,[-20643]=-28648,[-8799]=-4912,[-2943]=-18527,[-30432]=-2433,[-13673]=16103,[6623]=-25347,[-28572]=14407,[17173]=-5380,[-4285]=-5677,[30222]=14614,[-31850]=-28489,[24818]=7004,[20190]=-32203,[-20066]=4785,[4641]=26136,[25988]=13234,[-13187]=-10125,[24991]=-13532,[32309]=-10309,[9281]=-30386,[16278]=-6954,[-17320]=-28144,[6712]=-5074,[8030]=-24649,[-14247]=2314,[18189]=-19380,[12438]=10119,[8649]=24086,[28979]=3970,[23810]=30500,[26251]=-476,[-5642]=14188,[6311]=6178,[-19811]=-19927,[31417]=14614,[15385]=22154,[-7517]=-9743,[32668]=-25700,[-10930]=14614,[12896]=4796,[-17324]=-1541,[26885]=-24089,[-12387]=-27001,[2341]=-19256,[18832]=-27315,[-17619]=22601,[-24745]=-4294,[-5722]=-9207,[-27334]=-8861,[-32541]=28182,[-19555]=-4294,[-31275]=11876,[15295]=-8132,[-20423]=14614,[-16935]=17876,[19589]=14614,[17629]=31173,[11804]=2847,[12634]=14614,[-31780]=14614,[-19332]=3054,[-30041]=-27173,[23205]=-3499,[-24958]=7842,[-19547]=45,[-19998]=11976,[-28514]=28899,[13644]=12410,[22643]=-5844,[-23680]=-7372,[-1666]=-19380,[-30189]=19533,[3296]=28173,[-27405]=11865,[-10243]=16723,[15386]=-32278,[-19896]=-2566,[-8318]=-32589,[-13473]=-13125,[-32169]=-2377,[-14571]=-12798,[2988]=2847,[13492]=14614,[14965]=-18472,[280]=3898,[6365]=10562,[19325]=6725,[-6879]=17357,[-21184]=-22940,[-12576]=29885,[15099]=-1584,[-2945]=14614,[-13171]=7474,[30798]=3158,[-22737]=-19256,[-7089]=-28733,[14962]=20960,[-29109]=29663,[-14080]=-24662,[-25115]=-13454,[18947]=8071,[-26060]=-6739,[-26124]=30092,[29949]=7221,[-18689]=-16789,[-17793]=12410,[16607]=14614,[8113]=-4381,[-15230]=13278,[11805]=-8510,[-6464]=6874,[23512]=10732,[-2640]=15109,[-26179]=25893,[19077]=15777,[22875]=-24576,[4903]=14614,[24576]=14614,[-29136]=-23157,[-26613]=20231,[-13874]=-7585,[-9141]=25343,[130]=29885,[-19095]=14614,[18238]=-20572,[23742]=14614,[-4067]=-13841,[10340]=-32723,[27720]=10898,[30715]=-22038,[-13192]=11479,[-21758]=4608,[-20682]=32099,[-5038]=14614,[-8992]=26136,[10443]=28489,[-19834]=21288,[21184]=5698,[-27599]=17441,[-27561]=15343,[-31831]=31242,[-11803]=6899,[-21596]=-20203,[-7423]=-4381,[30217]=5968,[-2690]=24331,[3319]=11634,[11158]=-27930,[28487]=-27271,[21134]=-18193,[17324]=5596,[-27638]=7004,[-22562]=-6624,[4254]=3898,[29138]=-31288,[-1746]=30954,[9023]=-2829,[10210]=-30386,[20871]=7201,[13238]=-19142,[-9104]=8792,[-13040]=28604,[-29873]=-24187,[19217]=-5429,[-17587]=25281,[-3593]=28433,[16575]=-6739,[-27480]=17873,[-32542]=-13766,[-5424]=-15821,[5868]=-14767,[-17196]=23020,[-2533]=-11141,[23961]=30359,[-2804]=20043,[-31702]=18773,[-7023]=-25006,[20444]=6261,[-32706]=-10309,[26301]=30500,[-7647]=-27652,[25569]=-9670,[-21921]=-21881,[-6596]=-7972,[30247]=-10309,[26371]=-1224,[17253]=-31288,[-5129]=28669,[155]=21544,[9419]=-22973,[24249]=11031,[6927]=14614,[-23033]=14614,[-20112]=3898,[-29190]=-18527,[-25015]=946,[32606]=28290,[-15327]=-27745,[4012]=-8723,[5027]=-2183,[-18603]=-16106,[-26851]=-4381,[19995]=693,[-27350]=14614,[-22166]=-31246,[19648]=14614,[19118]=-14767,[-31089]=-9595,[-4420]=14614,[-23288]=11365,[-22927]=30959,[13145]=24905,[-28055]=-26348,[1759]=-21526,[32763]=3074,[21931]=-26087,[24530]=5696,[23132]=-14131,[-19840]=14614,[-14258]=4060,[-17968]=392,[31724]=21544,[-4687]=-8617,[23217]=15089,[19269]=9110,[6778]=19064,[-10800]=-11386,[28009]=14614,[27323]=-5844,[-12452]=1871,[-4944]=-1568,[-20882]=27852,[8992]=8179,[5101]=-1678,[4909]=-31106,[18972]=-2865,[30984]=-29522,[-22590]=18773,[-24491]=-5380,[-4262]=1997,[-8913]=-8654,[-5720]=-10309,[-7265]=-2090,[-9421]=-24325,[-27622]=-9670,[-21577]=1843,[-26912]=-14435,[22905]=-15716,[-1258]=-25072,[-1064]=31033,[20303]=7201,[-32580]=-18532,[14098]=12410,[20518]=21987,[-6165]=31297,[4349]=14614,[19385]=14614,[-29341]=31096,[14186]=9553,[-25029]=29313,[-30700]=14614,[28238]=14614,[-8695]=14614,[11001]=10119,[8027]=-8479,[-15566]=2314,[-30262]=22275,[20669]=19727,[-6137]=-10659,[20792]=-16470,[-9367]=-12941,[-7573]=17033,[17861]=5748,[19626]=-22719,[15532]=12018,[-2452]=-24287,[31728]=-16106,[1905]=24499,[-28993]=20531,[-15604]=-355,[-12791]=18667,[22829]=14614,[-31193]=13991,[-25550]=-19281,[25000]=-30641,[-30138]=9110,[2496]=20519,[1998]=26746,[20791]=31033,[2470]=24499,[26675]=32099,[-29324]=47,[1301]=-8166,[21844]=-31288,[2513]=-27331,[-16273]=-8344,[16195]=-4171,[-20143]=-20042,[-4709]=-1905,[30070]=11798,[4642]=14614,[-8547]=-10309,[9001]=-27331,[-7903]=28669,[-2406]=-22561,[5136]=-11564,[-28995]=23496,[12596]=-30811,[27398]=19064,[-28977]=-31559,[14276]=-30195,[-6824]=-2687,[-21817]=-14441,[-3447]=20254,[8448]=-15815,[-16739]=-26978,[-22633]=15089,[1637]=1284,[-21760]=8107,[3225]=-15821,[14297]=5821,[-19029]=25865,[15578]=15211,[-9333]=-10684,[11690]=-20419,[31716]=18273,[10674]=-8810,[-13710]=18824,[2885]=25281,[-25394]=28899,[-17844]=-25640,[29416]=-25006,[-2343]=3618,[19373]=-5677,[13462]=22602,[-18629]=-24287,[-11092]=26129,[10828]=-12951,[-20491]=22154,[-23784]=-23719,[23700]=-6673,[-29241]=27196,[10065]=-28496,[29767]=320,[22316]=31309,[16118]=-27315,[-16298]=14614,[-13371]=8792,[1216]=-20124,[3244]=-17935,[26416]=-14604,[19803]=-26735,[7823]=2480,[-16449]=13884,[-12458]=14614,[17949]=3052,[13656]=5703,[-30534]=14614,[-27296]=-15441,[-14222]=-6373,[7953]=-348,[26807]=30959,[10236]=13518,[-22542]=5575,[15016]=-24325,[-24838]=12410,[24430]=24647,[-1715]=-32098,[26612]=-26199,[-22748]=-27453,[-6398]=31242,[-23478]=-32028,[5832]=-22038,[21297]=20960,[24616]=11984,[10276]=5685,[20787]=-17276,[-7926]=14614,[31452]=11031,[32019]=25281,[-6801]=18773,[15070]=14227,[-23266]=-4381,[-16389]=-15815,[11986]=-31288,[2023]=-9471,[27173]=-7585,[18082]=-14958,[-27776]=-22397,[-9633]=7842,[23852]=-32376,[-28930]=32670,[6143]=-520,[7164]=21791,[26746]=-29351,[-161]=3898,[-26904]=-21290,[-17676]=27852,[2308]=1873,[-7118]=14614,[-3735]=16663,[18283]=9704,[-7552]=-1399,[29348]=-17112,[24956]=-27173,[1694]=-27331,[-25776]=3970,[-32646]=6874,[14642]=-25787,[-11756]=-31410,[-31951]=-21199,[-15626]=15958,[-31451]=15777,[-5233]=16870,[-15475]=-2687,[-30879]=24340,[-8488]=-15815,[21313]=24169,[-26474]=25287,[-10532]=-4120,[-23489]=16665,[-23523]=-15821,[5164]=14614,[22184]=-16470,[30392]=-2377,[17975]=8326,[27318]=-7652,[-19665]=27894,[-15491]=14614,[18703]=19014,[30057]=14614,[9975]=14614,[7952]=-4972,[-29111]=31420,[-14481]=-2604,[-19827]=32099,[-28370]=-6158,[3525]=-3643,[-26659]=10739,[21757]=28182,[-28937]=-16294,[27310]=-2433,[28484]=-9056,[13517]=-17112,[-23073]=-15716,[-21670]=-4294,[25971]=25281,[-32164]=3565,[28765]=25281,[-13149]=-22561,[9003]=14614,[11911]=32466,[28920]=20976,[25203]=3054,[8266]=25865,[-6716]=8621,[1681]=-24812,[-3875]=-15821,[-23752]=-24254,[-436]=14614,[-25290]=-2410,[14073]=-4151,[-2957]=-9207,[-2683]=18273,[2728]=16239,[31748]=3158,[28040]=-17404,[-14536]=-31378,[-4068]=6463,[24941]=12591,[-21841]=-1232,[16457]=-4381,[-435]=-12951,[-619]=15958,[6768]=9383,[-16023]=-12766,[9203]=-12951,[-14178]=-572,[19178]=25138,[20153]=-4683,[27665]=31341,[21409]=-28613,[-14465]=14614,[27467]=19500,[-30921]=20243,[-18553]=14614,[-22445]=-11788,[17339]=26694,[19358]=-27842,[16938]=26622,[29453]=-5013,[32147]=3618,[2416]=-10659,[-2016]=21919,[-1793]=6198,[30349]=9017,[7893]=-28991,[9982]=25281,[28197]=23404,[-25493]=20323,[28609]=22154,[-9455]=-29890,[-8242]=-21526,[-31107]=16663,[22681]=-2410,[-16989]=19872,[21803]=-2517,[3606]=-16260,[-14347]=19872,[609]=-31400,[18336]=-1481,[29475]=-6919,[4733]=14614,[27492]=-23343,[22155]=23938,[7284]=15914,[32368]=14614,[4162]=19588,[6099]=-16106,[-30865]=31263,[-3236]=237,[30811]=31576,[-26861]=29023,[-8974]=-12951,[-26377]=17357,[12499]=-17074,[1888]=32099,[6982]=-1541,[13874]=25281,[-24347]=-25680,[1300]=30816,[-24509]=-26350,[-26969]=-9379,[-10712]=-32203,[9388]=28260,[-20703]=-29915,[3576]=10847,[29095]=-10309,[27439]=-7471,[-31180]=-5656,[-25943]=-17668,[-17000]=-20042,[-20871]=21987,[-7457]=32099,[-5369]=25281,[-14736]=26674,[-8439]=-24745,[12926]=14614,[8432]=23276,[6346]=11356,[14895]=21998,[31550]=-21199,[8369]=-20124,[-27431]=29640,[14076]=26078,[9752]=11984,[-9368]=-32278,[-10755]=24522,[-7986]=22154,[-11042]=1252,[-9110]=14614,[31632]=-16441,[-28095]=-30195,[-8319]=14614,[-7939]=14614,[4934]=-6225,[13194]=-27701,[23924]=190,[24442]=30500,[-2990]=9039,[14077]=-24745,[25406]=28290,[-14730]=5696,[7064]=25913,[1990]=-2865,[-3506]=16234,[13202]=-2836,[32289]=31380,[-21154]=-2719,[-24053]=14614,[5281]=-4294,[-24494]=4683,[18977]=-20124,[10898]=22154,[25801]=21048,[4608]=2966,[-14446]=11876,[-25495]=-11141,[23326]=-15122,[29145]=21571,[19074]=31309,[29429]=-28587,[-3509]=-2865,[6506]=-25452,[-21175]=26522,[14310]=14186,[-32650]=16899,[22932]=24905,[18266]=14407,[-20691]=11984,[16601]=23768,[2903]=5703,[28356]=31172,[22183]=-2405,[22992]=-9936,[13797]=11798,[886]=12410,[22841]=5101,[8137]=-5677,[-17666]=14210,[2076]=12094,[-22170]=11031,[-29178]=-15938,[4147]=-27672,[32757]=20492,[-569]=14614,[-19272]=27865,[270]=-20063,[-22024]=-7461,[15468]=-29915,[15137]=-12804,[24556]=-4798,[-16779]=14614,[-8367]=27620,[-29786]=-11788,[-3306]=14188,[17970]=31096,[11443]=-84,[-19522]=-24703,[17158]=-1584,[12097]=-15607,[-1821]=-6779,[27465]=-31559,[-5430]=-6225,[-20912]=15958,[-6519]=-12951,[-9959]=21998,[-16428]=26210,[2345]=31186,[-1354]=-252,[16084]=-29351,[29968]=14614,[28666]=5657,[-26075]=-24089,[22157]=-776,[17838]=14614,[-18562]=-4683,[16097]=31819,[1042]=-22346,[-2444]=-26033,[-31675]=878,[-12609]=-15594,[14777]=22513,[-6765]=13211,[-3602]=-1941,[17215]=14614,[4177]=-17112,[-19633]=5113,[30684]=24522,[-4485]=25913,[-25222]=29851,[7963]=-31400,[-17478]=-28613,[-10315]=18667,[11060]=-25676,[-10749]=-4798,[-17710]=25281,[4833]=-992,[-20667]=19064,[28445]=392,[-29421]=10709,[7431]=61,[32421]=488,[-4478]=-24287,[-5357]=9704,[-24434]=9573,[-12847]=-10125,[-14713]=-5677,[-12677]=-7172,[-4092]=15914,[5108]=-23577,[-2062]=29555,[-28833]=17709,[19032]=20992,[-19578]=2372,[27997]=-23577,[-31724]=-383,[9847]=25287,[-9063]=-5429,[-20674]=6874,[3248]=14614,[-30845]=-5074,[26293]=16575,[8472]=-4294,[30981]=26674,[-16650]=-10600,[-23106]=-1399,[-15931]=-1941,[2707]=4608,[-941]=1871,[-19398]=-12951,[24602]=-30505,[31558]=-6673,[-27814]=32523,[3449]=31542,[2486]=-26033,[13254]=-17187,[2975]=-4373,[22924]=6837,[-11883]=5328,[3069]=-880,[2631]=14614,[-22415]=16103,[-23829]=22602,[-23007]=-22346,[13449]=-32132,[-31830]=14614,[18262]=19533,[-9260]=-12951,[-4714]=-21273,[32192]=21791,[230]=14614,[-29685]=21579,[7793]=26762,[21643]=-851,[29683]=-10044,[-32417]=14614,[-7405]=-4415,[-2861]=-4942,[5864]=11145,[-3340]=28173,[13763]=3250,[-27533]=342,[-12727]=-22719,[27192]=8792,[-14317]=14614,[24081]=-16236,[14327]=27831,[30325]=-15938,[29193]=12410,[-15833]=-1541,[30842]=14614,[-13469]=-5677,[2019]=-17207,[-29043]=28489,[4743]=38,[20188]=5698,[-2644]=13278,[-2926]=14614,[19463]=-4798,[-12960]=-7944,[-13103]=14614,[15657]=-24287,[27370]=14227,[-12539]=-2566,[-2626]=1602,[7643]=-10600,[-8839]=31380,[16518]=5531,[-8658]=-14150,[9964]=-1024,[8885]=-992,[18667]=25281,[-3911]=29789,[-5863]=-29077,[-5431]=27894,[-14061]=-32376,[2435]=-1678,[-25741]=-17754,[-13991]=14614,[-13806]=-422,[15151]=22059,[-21730]=15405,[23595]=31186,[24404]=29313,[7351]=28489,[-2464]=-28331,[16633]=14614,[12227]=-27791,[-21284]=14614,[-19330]=14614,[17286]=13456,[27073]=-1841,[-19740]=14614,[-24558]=14614,[21869]=25287,[28411]=-8810,[18205]=-27331,[-2920]=14614,[-20269]=14614,[-21872]=28604,[25021]=25281,[-12751]=17002,[-15320]=-20996,[-1432]=23967,[-15991]=14614,[18845]=-27842,[-4562]=20724,[30845]=-25640,[26271]=-4972,[-25971]=-30250,[-28207]=18201,[22542]=-26348,[4301]=-14568,[23890]=10898,[6227]=-15821,[-14986]=-31400,[-27574]=7582,[23846]=-19298,[-8901]=-15815,[6691]=-5458,[6229]=-12766,[-138]=-28496,[11044]=7520,[3981]=30092,[-28204]=5657,[-27271]=23593,[4419]=-476,[27911]=11515,[18199]=-27676,[14013]=-31782,[4418]=28699,[-24003]=-15248,[15104]=6611,[18370]=10594,[-30133]=15958,[1882]=19502,[4632]=5210,[4968]=-30250,[7568]=-31159,[32214]=26073,[8668]=-30392,[17082]=11508,[11376]=14614,[-12892]=2766,[-26749]=22513,[-10038]=-30392,[-791]=-851,[-12564]=-2525,[-8759]=-32432,[16813]=-32481,[-3809]=11707,[-31594]=-31288,[-27097]=-15815,[-7279]=-25295,[16463]=14614,[-28940]=-20363,[660]=-3616,[-9488]=14614,[15804]=29274,[-11811]=14614,[-31322]=28760,[27423]=14614,[20102]=14614,[-15723]=14614,[3325]=4750,[11175]=14614,[-14579]=-4294,[-1391]=14614,[22418]=-29436,[31173]=4608,[9546]=-1399,[-7130]=30643,[8693]=24171,[-13604]=-4108,[-10376]=-12579,[5801]=-16106,[-20761]=11707,[14735]=14614,[-18034]=32099,[12769]=14614,[15958]=2966,[-30595]=22154,[-12439]=237,[26376]=-27930,[-10929]=14016,[3027]=19064,[1479]=29640,[11461]=-32507,[10140]=-3616},function(e)
                return jb[e+-273]
            end
            Bc={[-17668]=function()
                Q[1]=ad[Q[2]]
                ba=ae(-27782)
            end,[-25787]=function()
                Q[1]=Q[3][Q[1]]
                ba=-10896
            end,[30897]=function()
                Q[4]=-28909
                ba=ae(-27301)
            end,[38]=function()
                Q[4]=-28909
                ba=27021
            end,[-21881]=function()
                Q[1]=-28909
                ba=-14823
            end,[18201]=function()
                Q[1]=1
                ba=ae(-23556)
            end,[-14441]=function()
                Q[5]=29290
                ba=ae(-31402)
            end,[-2829]=function()
                Q[1]=Q[6][Q[1]]
                ba=5923
            end,[-26735]=function()
                Q[1]=xc[Q[7]]
                ba=ae(-7244)
            end,[-29835]=function()
                Q[1]=bb(Qc(Q[8]))
                ba=ae(-12166)
            end,[-15845]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(20426)
            end,[342]=function()
                ad[Q[2]]=Q[1];
                ba=ae(7200)
            end,[-26261]=function()
                Q[12]={__mode=Q[12]}
                ba=ae(23148)
            end,[-5844]=function()
                Q[2]-=Q[1];
                ba=2942;
            end,[17873]=function()
                Q[1]=Q[6][Q[1]]
                ba=ae(2349)
            end,[-14958]=function()
                Ab[Q[3][-22190]]=se(Q[1][1],1,Q[1][2]);
                Q[14],Q[15],Q[16]=(Q[8])+151,1,152
                ba=-12909
            end,[-18135]=function()
                Q[1]=Q[1][Q[17]]
                ba=ae(-9146)
            end,[-28931]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(27738)
            end,[-5189]=function()
                Q[9]=158
                ba=ae(15805)
            end,[-1568]=function()
                Q[1]=15917
                ba=ae(14915)
            end,[1871]=function()
                Q[18],Q[19]=se(Q[1][1],1,Q[1][2]);
                if not Q[18]then
                    ba=ae(-9148)
                    return true
                end
                ba=ae(19127)
            end,[-8617]=function()
                Q[1]=3
                ba=17629
            end,[-19380]=function()
                Q[8]=Q[1];
                ba=-29835;
            end,[15109]=function()
                Q[11]=-22190
                ba=ae(-5892)
            end,[12591]=function()
                Q[6][-10759]=se(Q[1][1],1,Q[1][2]);
                ba=-16660;
            end,[-28113]=function()
                Q[9]=Q[11]
                ba=-31003
            end,[-1584]=function()
                Q[1]=3800
                ba=31973
            end,[6725]=function()
                Q[13]=0
                ba=ae(-29600)
            end,[-27271]=function()
                Q[1]=1
                ba=ae(22916)
            end,[-10684]=function()
                Q[11]=Q[3][Q[11]]
                ba=ae(-13331)
            end,[-23577]=function()
                Q[20][3]=Q[1];
                ba=ae(-20609);
            end,[13793]=function()
                Q[9]=242
                ba=25240
            end,[-22346]=function()
                ad[Q[2]]=Q[1];
                ba=ae(-22760)
            end,[-2338]=function()
                Q[12]=-28909
                ba=28267
            end,[-22940]=function()
                Q[5]=29290
                ba=-29568
            end,[-26978]=function()
                Q[4]=Q[3][Q[4]]
                ba=ae(19451)
            end,[21998]=function()
                Q[21]=Q[1];
                ba=ae(-25277);
            end,[-6315]=function()
                Q[1]=Q[6][Q[1]]
                ba=22487
            end,[-13863]=function()
                Q[9]=25
                ba=ae(-13533)
            end,[-2566]=function()
                Q[22][(Q[23]-151)]=Q[1];
                ba=ae(-2605)
            end,[-8810]=function()
                Q[1]=ad[Q[2]]
                ba=ae(-26476)
            end,[-16979]=function()
                Q[9]=63
                ba=ae(17086)
            end,[10249]=function()
                Q[12]=-28909
                ba=ae(-1914)
            end,[10709]=function()
                Q[6][15917]=se(Q[1][1],1,Q[1][2]);
                ba=10803;
            end,[-19948]=function()
                Q[9]=Q[3][Q[9]]
                ba=8255
            end,[-6919]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(27019)
            end,[-1841]=function()
                Q[12]='ks'
                ba=-26261
            end,[11546]=function()
                Q[11]=-22190
                ba=ae(-9060)
            end,[5101]=function()
                Q[10]=-22190
                ba=ae(-2367)
            end,[-6941]=function()
                Q[4]=-28909
                ba=ae(-12619)
            end,[7957]=function()
                Q[10]=3
                ba=ae(-16948)
            end,[22487]=function()
                Q[9]=31907
                ba=-27200
            end,[-383]=function()
                Q[4]=Q[3][Q[4]]
                ba=ae(-20911)
            end,[6886]=function()
                Q[10]=-22190
                ba=ae(2271)
            end,[25138]=function()
                Q[5]=29290
                ba=10134
            end,[-11531]=function()
                Q[1]=28498
                ba=ae(8226)
            end,[-1678]=function()
                Q[20][1]=Q[1];
                ba=ae(-7969);
            end,[8482]=function()
                Q[9]=Q[3][Q[9]]
                ba=9769
            end,[-14767]=function()
                Q[2]+=Q[1];
                ba=ae(-19467)
            end,[25240]=function()
                Q[1]=aa(Q[1],Q[9])
                ba=ae(-25017)
            end,[-15900]=function()
                Q[9]=Ab[Q[9]]
                ba=7957
            end,[13234]=function()
                Q[9]=31907
                ba=ae(2292)
            end,[14227]=function()
                Q[1]=1
                ba=32295
            end,[-17207]=function()
                Q[9]=Q[3][Q[9]]
                ba=-17327
            end,[5167]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(-27260)
            end,[7023]=function()
                Q[1]=28498
                ba=ae(8066)
            end,[-32376]=function()
                Q[1]=bb(lc(Q[24]))
                ba=ae(-26104)
            end,[-5380]=function()
                Q[1]=Ab[Q[23]]
                ba=ae(-20430)
            end,[25865]=function()
                Q[25]=Q[1];
                ba=ae(2689);
            end,[-7434]=function()
                Q[1]=-10759
                ba=ae(15851)
            end,[-21526]=function()
                Q[20][2]=Q[20];
                ba=-11243;
            end,[23020]=function()
                Q[1]=1
                ba=ae(-31678)
            end,[-2183]=function()
                Q[5]=29290
                ba=-15113
            end,[-32432]=function()
                Q[1]=1
                ba=ae(9665)
            end,[-29240]=function()
                Q[1]=28498
                ba=ae(-26562)
            end,[13884]=function()
                Q[1]=bb(ze(Q[26],Q[22]))
                ba=ae(18355)
            end,[-24960]=function()
                Q[1]=Ab[Q[27]]
                ba=ae(-18756)
            end,[11479]=function()
                Q[11]=Q[10]
                ba=-28113
            end,[32466]=function()
                Q[1]=-22190
                ba=ae(28470)
            end,[-28648]=function()
                Q[1]=-22190
                ba=ae(15400)
            end,[-27001]=function()
                Q[10]=-22190
                ba=-4464
            end,[-32028]=function()
                Q[1]=21753
                ba=-26869
            end,[13004]=function()
                Q[9]=Q[9]-Q[10]
                ba=ae(21916)
            end,[16620]=function()
                Q[1]=28498
                ba=-5189
            end,[-16660]=function()
                Q[1]=80
                ba=ae(-7630)
            end,[21919]=function()
                Q[1]=bb(Q[1](Q[28]))
                ba=ae(2726)
            end,[5703]=function()
                Q[2]-=Q[1];
                ba=ae(11317);
            end,[-19256]=function()
                Q[6]=Q[1];
                ba=-7434;
            end,[-6158]=function()
                Q[1]=bb(ze(Q[26],Q[22]))
                ba=ae(-14074)
            end,[-17187]=function()
                Q[1]=3800
                ba=1305
            end,[-27200]=function()
                Q[9]=Q[3][Q[9]]
                ba=-25801
            end,[28899]=function()
                Q[1]=15917
                ba=-21774
            end,[-17074]=function()
                if Q[29]==1 then
                    ba=ae(-1818)
                    return true
                elseif not(Q[29]==2)then
                    ba=ae(-6246)
                    return true
                else
                    ba=ae(2296)
                    return true
                end
                ba=ae(9253)
            end,[-30250]=function()
                Q[2]+=Q[1];
                ba=ae(16370);
            end,[-31246]=function()
                Q[1]=-Q[1]
                ba=ae(-12876)
            end,[-24187]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(16051)
            end,[13456]=function()
                ad[Q[2]]=Q[1];
                ba=ae(17488)
            end,[-9207]=function()
                Q[30],Q[27],Q[31]=Q[1],Q[9],se(Q[10][1],1,Q[10][2]);
                ba=ae(-28836);
            end,[-29704]=function()
                Q[1]=bb(Qc(Q[8]))
                ba=ae(30343)
            end,[8927]=function()
                Q[4]=-28909
                ba=ae(-16466)
            end,[-27930]=function()
                Q[32],Q[33]=Q[1],Q[9];
                ba=-12232;
            end,[29663]=function()
                Q[1]=0
                ba=ae(-2417)
            end,[5531]=function()
                Q[4]=-28909
                ba=ae(-28722)
            end,[5698]=function()
                Q[2]-=Q[1];
                ba=-11531;
            end,[28760]=function()
                Q[11]=Q[3][Q[11]]
                ba=ae(-31577)
            end,[-25060]=function()
                Q[9]=Q[3][Q[9]]
                ba=8038
            end,[-14568]=function()
                Q[19]=Q[1];
                Q[34],Q[35],Q[36]=Q[31],1,Q[27]+1
                ba=8634
            end,[28848]=function()
                Q[1]=bb((function(nb,bd)
                    local Ta,yc,Wa,lb,Yd
                    yc={}
                    Wa,Yd={[21772]=28215,[-19776]=12532,[-1467]=3549,[-30125]=25120,[23283]=1428,[-27955]=-20811,[10402]=12532,[4110]=-11937,[-7664]=-8703,[-15657]=3549,[1041]=21371,[-13858]=-30942,[1128]=22023,[1135]=13736,[-31112]=12532,[26650]=-3334,[-28677]=12532,[-23122]=20925,[23762]=12532,[1234]=25120,[1952]=9368,[-583]=-6590,[4070]=-3075},function(Xb)
                        return Wa[Xb-4809]
                    end
                    lb={[-20811]=function()
                        yc[1]=142
                        Ta=Yd(-9049)
                    end,[9368]=function()
                        yc[2]=yc[2]+yc[3];
                        yc[4]=yc[2];
                        if yc[2]~=yc[2]then
                            Ta=Yd(-14967)
                        else
                            Ta=Yd(31459)
                        end
                    end,[16228]=function()
                        yc[5]=(yc[5])%yc[6]
                        Ta=-13424
                    end,[13736]=function()
                        yc[5]=bb(ac(bd,yc[5]))
                        Ta=30888
                    end,[-6590]=function()
                        yc[5]=142
                        Ta=16904
                    end,[11791]=function()
                        yc[5]=1
                        Ta=-8038
                    end,[-30942]=function()
                        yc[1]=yc[4]-yc[1]
                        Ta=11791
                    end,[-13424]=function()
                        yc[6]=1
                        Ta=Yd(8879)
                    end,[-3075]=function()
                        yc[5]=yc[5]+yc[6]
                        Ta=Yd(5944)
                    end,[-8038]=function()
                        yc[1]=(yc[1])+yc[5]
                        Ta=-18666
                    end,[25120]=function()
                        yc[7]=yc[1];
                        Ta=Yd(6761)
                    end,[1428]=function()
                        yc[6]=#bd
                        Ta=16228
                    end,[-18666]=function()
                        yc[1]=ac(nb,yc[1])
                        Ta=Yd(4226)
                    end,[-2206]=function()
                        yc[1]=yc[7]..se(yc[1][1],1,yc[1][2])
                        Ta=Yd(-25316)
                    end,[22023]=function()
                        yc[1]=''
                        Ta=Yd(-10848)
                    end,[21371]=function()
                        yc[4]=yc[2];
                        if yc[8]~=yc[8]then
                            Ta=Yd(28571)
                        else
                            Ta=-3334
                        end
                    end,[16904]=function()
                        yc[5]=yc[4]-yc[5]
                        Ta=Yd(28092)
                    end,[883]=function()
                        yc[1]=bb(zd(se(yc[1][1],1,yc[1][2])))
                        Ta=-2206
                    end,[30888]=function()
                        yc[1]=bb(aa(yc[1],se(yc[5][1],1,yc[5][2])))
                        Ta=883
                    end,[-3334]=function()
                        if(yc[3]>=0 and yc[2]>yc[8])or((yc[3]<0 or yc[3]~=yc[3])and yc[2]<yc[8])then
                            Ta=Yd(15211)
                        else
                            Ta=Yd(-23146)
                        end
                    end}
                    Ta=Yd(5937)
                    repeat
                        while true do
                            yc[9]=lb[Ta]
                            if yc[9]~=nil then
                                if yc[9]()then
                                    break
                                end
                            elseif Ta==3549 then
                                yc[7]=yc[1];
                                yc[2],yc[8],yc[3]=142,(#nb-1)+142,1
                                Ta=Yd(5850)
                            elseif Ta==12532 then
                                return yc[7]
                            end
                        end
                    until Ta==7967
                end)(Q[1],Q[9]))
                ba=ae(-27822)
            end,[-4108]=function()
                Q[12]=-28909
                ba=ae(16791)
            end,[-2687]=function()
                ad[Q[2]]=Q[1];
                ba=ae(13042)
            end,[-10896]=function()
                Q[1]=Q[37][Q[1]]
                ba=ae(-15750)
            end,[-27275]=function()
                Q[4]=Q[3][Q[4]]
                ba=-7180
            end,[11876]=function()
                Q[1]=15917
                ba=-1383
            end,[-20063]=function()
                if(Q[38]>=0 and Q[39]>Q[40])or((Q[38]<0 or Q[38]~=Q[38])and Q[39]<Q[40])then
                    ba=ae(-19996)
                else
                    ba=ae(-25670)
                end
            end,[28173]=function()
                Q[41]=Q[1];
                ba=ae(27938);
            end,[8071]=function()
                Q[10]=cc(Q[10],Q[11])
                ba=ae(-6816)
            end,[20976]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(22589)
            end,[28699]=function()
                Q[1]=1
                ba=ae(-21893)
            end,[15416]=function()
                Q[1]=28498
                ba=ae(-32377)
            end,[-20042]=function()
                Q[1]=ad[Q[2]]
                ba=ae(-22464)
            end,[26129]=function()
                Q[9]=-10759
                ba=-19948
            end,[28267]=function()
                Q[4]=-28909
                ba=-26981
            end,[29789]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11]}
                ba=ae(27080)
            end,[-4312]=function()
                Q[9]=1
                ba=ae(-30920)
            end,[19064]=function()
                Q[22][(Q[23]-161)]=Q[42];
                ba=ae(-19125)
            end,[31819]=function()
                Q[1]=-22190
                ba=8904
            end,[-7972]=function()
                Q[1]=bb(aa(Q[1],Q[9]))
                ba=3461
            end,[-9670]=function()
                Q[30],Q[27],Q[43]=Q[1],Q[9],Q[10];
                ba=-24960;
            end,[-7944]=function()
                if Q[44]==217 then
                    ba=ae(-22475)
                    return true
                elseif Q[44]==157 then
                    ba=ae(-19870)
                    return true
                elseif Q[44]==135 then
                    ba=ae(28629)
                    return true
                elseif not(Q[44]==141)then
                    ba=ae(-10121)
                    return true
                else
                    ba=ae(12184)
                    return true
                end
                ba=ae(-4147)
            end,[15777]=function()
                Q[1]=1
                ba=ae(-2171)
            end,[9017]=function()
                Q[45][Q[23]]=nil;
                ba=ae(29038)
            end,[-25801]=function()
                Q[1]=bb((function(Gd,sa)
                    local Nc,pd,_d,qd,u
                    qd={}
                    pd,_d={[3307]=-22888,[-24162]=-22888,[27821]=-2828,[21232]=-22888,[26508]=-26675,[17975]=8415,[-19987]=-13310,[-7911]=9183,[21132]=14448,[-24918]=-2828,[21317]=21269,[14619]=12737,[-25260]=-22888,[18888]=12737,[28874]=-8315,[9200]=-13310,[1752]=8415,[9068]=-25418,[-27481]=-29425,[-31420]=-8315,[8154]=-22888,[5224]=28436,[21595]=-22256,[-18952]=30193},function(we)
                        return pd[we+17292]
                    end
                    Nc={[14448]=function()
                        qd[1]=qd[2]..se(qd[1][1],1,qd[1][2])
                        u=_d(-42210)
                    end,[11536]=function()
                        qd[1]=ac(Gd,qd[1])
                        u=12885
                    end,[9183]=function()
                        qd[3]=qd[4]-qd[3]
                        u=-32451
                    end,[30655]=function()
                        qd[1]=qd[4]-qd[1]
                        u=6936
                    end,[6513]=function()
                        qd[1]=(qd[1])+qd[3]
                        u=11536
                    end,[12885]=function()
                        qd[3]=195
                        u=_d(-25203)
                    end,[-25418]=function()
                        qd[5]=qd[5]+qd[6];
                        qd[4]=qd[5];
                        if qd[5]~=qd[5]then
                            u=_d(-13985)
                        else
                            u=-2224
                        end
                    end,[6936]=function()
                        qd[3]=1
                        u=6513
                    end,[30193]=function()
                        qd[4]=qd[5];
                        if qd[7]~=qd[7]then
                            u=_d(-9138)
                        else
                            u=-2224
                        end
                    end,[-8315]=function()
                        qd[1]=''
                        u=_d(-37279)
                    end,[5681]=function()
                        qd[8]=1
                        u=-29483
                    end,[2889]=function()
                        qd[3]=(qd[3])%qd[8]
                        u=5681
                    end,[28436]=function()
                        qd[1]=bb(aa(qd[1],se(qd[3][1],1,qd[3][2])))
                        u=-3789
                    end,[-3789]=function()
                        qd[1]=bb(zd(se(qd[1][1],1,qd[1][2])))
                        u=_d(3840)
                    end,[-32451]=function()
                        qd[8]=#sa
                        u=2889
                    end,[-29483]=function()
                        qd[3]=qd[3]+qd[8]
                        u=_d(4025)
                    end,[21269]=function()
                        qd[3]=bb(ac(sa,qd[3]))
                        u=_d(-12068)
                    end,[-2828]=function()
                        qd[2]=qd[1];
                        u=_d(-8224)
                    end,[-2224]=function()
                        if(qd[6]>=0 and qd[5]>qd[7])or((qd[6]<0 or qd[6]~=qd[6])and qd[5]<qd[7])then
                            u=_d(3940)
                        else
                            u=_d(1596)
                        end
                    end,[12737]=function()
                        qd[1]=195
                        u=30655
                    end}
                    u=_d(-48712)
                    repeat
                        while true do
                            qd[9]=Nc[u]
                            if qd[9]~=nil then
                                if qd[9]()then
                                    break
                                end
                            elseif u==-13310 then
                                qd[2]=qd[1];
                                qd[7],qd[5],qd[6]=(#Gd-1)+195,195,1
                                u=_d(-36244)
                            elseif u==-22888 then
                                return qd[2]
                            end
                        end
                    until u==31227
                end)(Q[1],Q[9]))
                ba=ae(-30834)
            end,[6463]=function()
                Q[9]=Q[1]
                ba=-17634
            end,[-30386]=function()
                Ab[Q[30]]=Q[1];
                ba=-17072;
            end,[-31782]=function()
                Q[9]=1
                ba=1555
            end,[-21774]=function()
                Q[1]=Q[6][Q[1]]
                ba=ae(-19249)
            end,[-27315]=function()
                Q[2]-=Q[1];
                ba=16620;
            end,[-28358]=function()
                Q[1]=1
                ba=ae(23868)
            end,[3565]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(-22172)
            end,[-24788]=function()
                Q[10]=-22190
                ba=-22316
            end,[17918]=function()
                Q[10]=Q[10][Q[7]]
                ba=16441
            end,[-7892]=function()
                Q[9]=Q[20][Q[9]]
                ba=ae(18248)
            end,[3898]=function()
                Q[1]=216
                ba=ae(-11802)
            end,[22674]=function()
                Q[1]=1
                ba=ae(-21304)
            end,[23768]=function()
                Q[1]=1
                ba=ae(-4463)
            end,[-26087]=function()
                Q[1]=1
                ba=ae(19105)
            end,[6611]=function()
                Q[1]=Qa[Q[1]]
                ba=ae(-21487)
            end,[-8654]=function()
                Q[1]=Q[3][Q[1]]
                ba=13793
            end,[-6872]=function()
                Q[1]={[Q[1]]=Q[17],[Q[9]]=Ab}
                ba=ae(16231)
            end,[-16470]=function()
                Q[1]=Q[46].__iter
                ba=1312
            end,[16663]=function()
                Q[6][31907]=se(Q[1][1],1,Q[1][2]);
                ba=ae(4527)
            end,[-7372]=function()
                Q[1]=bb(y(Ud,Ab,Q[1],Q[27],Q[31]))
                ba=ae(-12179)
            end,[-15594]=function()
                Q[10]=3
                ba=ae(-2353)
            end,[-15815]=function()
                Q[16]=Q[16]+Q[15];
                Q[23]=Q[16];
                if Q[16]~=Q[16]then
                    ba=ae(31690)
                else
                    ba=6837
                end
            end,[29851]=function()
                Q[12]=-28909
                ba=8927
            end,[28510]=function()
                Q[11]=-22190
                ba=ae(-31049)
            end,[-4298]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(1315)
            end,[-27819]=function()
                Q[1]=28498
                ba=ae(-15331)
            end,[-31463]=function()
                ba=ae(30222);
                return true;
            end,[25281]=function()
                Q[23],Q[20]=Q[47](Q[48],Q[49]);
                Q[49]=Q[23];
                if Q[49]==nil then
                    ba=ae(-1113)
                else
                    ba=ae(28757)
                end
            end,[-25161]=function()
                Q[9]=1
                ba=-2229
            end,[-1481]=function()
                Q[9]=Q[3][Q[9]]
                ba=ae(6638)
            end,[392]=function()
                Q[9]=-28909
                ba=-32561
            end,[-8611]=function()
                Q[11]=Q[3][Q[11]]
                ba=15931
            end,[-5677]=function()
                Q[1]=-32174
                ba=ae(-26451)
            end,[190]=function()
                Q[36]=Q[36]+Q[35];
                Q[23]=Q[36];
                if Q[36]~=Q[36]then
                    ba=ae(-23250)
                else
                    ba=ae(-23687)
                end
            end,[8904]=function()
                Q[1]=Q[21][Q[1]]
                ba=ae(31997)
            end,[693]=function()
                ad[Q[2]]=Q[1];
                ba=ae(5006)
            end,[6837]=function()
                if(Q[15]>=0 and Q[16]>Q[14])or((Q[15]<0 or Q[15]~=Q[15])and Q[16]<Q[14])then
                    ba=ae(19862)
                else
                    ba=ae(-13905)
                end
            end,[-28496]=function()
                Q[46]=se(Q[1][1],1,Q[1][2]);
                if Q[46]~=nil and Q[46].__iter~=nil then
                    ba=ae(21065)
                    return true
                elseif Lc(Q[24])=='table'then
                    ba=ae(24125)
                    return true
                end
                ba=ae(-19282)
            end,[15931]=function()
                Q[12]=-28909
                ba=-29191
            end,[11365]=function()
                Q[17]=Q[1];
                ba=26519;
            end,[7932]=function()
                if not(Q[3][29290]==223)then
                    ba=ae(27765)
                    return true
                else
                    ba=ae(-28664)
                    return true
                end
                ba=ae(24849)
            end,[5685]=function()
                Q[50]=se(Q[1][1],1,Q[1][2]);
                ba=ae(-1473);
            end,[23496]=function()
                Q[4]=Q[3][Q[4]]
                ba=ae(-21544)
            end,[19727]=function()
                Q[10]=-10759
                ba=ae(-985)
            end,[27852]=function()
                Q[45][Q[23]]=nil;
                ba=ae(-21397)
            end,[12094]=function()
                Q[9]=-10759
                ba=ae(23785)
            end,[30643]=function()
                if not(Q[51]==3)then
                    ba=ae(-32433)
                    return true
                else
                    ba=ae(-31002)
                    return true
                end
                ba=ae(30520)
            end,[-23018]=function()
                Q[1]=Q[21][Q[1]]
                ba=-7536
            end,[18667]=function()
                Q[1]=Q[52].__iter
                ba=ae(11716)
            end,[14188]=function()
                ad[Q[2]]=Q[1];
                ba=ae(26238)
            end,[-20598]=function()
                Q[1]=te[Q[1]]
                ba=ae(-12266)
            end,[29523]=function()
                Q[9]=Q[2]+Q[9]
                ba=-1018
            end,[26762]=function()
                Q[9]=237
                ba=-6472
            end,[-19281]=function()
                Q[1]=1
                ba=ae(-2070)
            end,[29446]=function()
                if not(Q[44]==106)then
                    ba=ae(-1789)
                    return true
                else
                    ba=ae(-19392)
                    return true
                end
                ba=ae(-2672)
            end,[30959]=function()
                Q[53]=Q[1];
                ba=ae(-25851);
            end,[-11788]=function()
                Q[9]=-28909
                ba=12290
            end,[-26869]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(20555)
            end,[24905]=function()
                Q[42]=Q[1];
                if Q[42]==nil then
                    ba=ae(-4414)
                    return true
                end
                ba=ae(3300)
            end,[31147]=function()
                Q[9]=15738
                ba=ae(-6323)
            end,[-17404]=function()
                Q[10]=Q[54]-Q[30]
                ba=ae(-12919)
            end,[-13930]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11]}
                ba=ae(21064)
            end,[32157]=function()
                Q[4]=Q[3][Q[4]]
                ba=-23536
            end,[24647]=function()
                Q[2]+=Q[1];
                ba=ae(-10657)
            end,[-7969]=function()
                Q[9]=-28909
                ba=-29221
            end,[5811]=function()
                Q[10]=-22190
                ba=6246
            end,[21288]=function()
                Q[10]=Gc[Q[10]]
                ba=17918
            end,[-13454]=function()
                Q[11]=Q[3][Q[11]]
                ba=ae(-23511)
            end,[-8209]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(10413)
            end,[19493]=function()
                Q[9]=Q[11]
                ba=ae(25091)
            end,[11356]=function()
                Q[4]=Q[3][Q[4]]
                ba=27708
            end,[-2604]=function()
                Q[1]=13017
                ba=-4939
            end,[-422]=function()
                Q[10]=-22190
                ba=ae(19305)
            end,[17934]=function()
                Q[13]=0
                ba=ae(1954)
            end,[-348]=function()
                Q[9]=189
                ba=13366
            end,[17357]=function()
                Q[24],Q[55],Q[56]=se(Q[1][1],1,Q[1][2]);
                ba=ae(-481)
            end,[-25295]=function()
                Q[9]=Q[21][Q[9]]
                ba=ae(-14128)
            end,[5151]=function()
                Q[1]=Q[3][Q[1]]
                ba=31147
            end,[5748]=function()
                Q[4]=-28909
                ba=-27275
            end,[30039]=function()
                if Q[9]then
                    ba=ae(-27365)
                    return true
                end
                ba=ae(16551)
            end,[-21490]=function()
                Q[11]=-22190
                ba=ae(-26696)
            end,[-9936]=function()
                Q[1]=-28909
                ba=ae(3248)
            end,[-26199]=function()
                Q[3][28498]=Q[1];
                ba=533;
            end,[28182]=function()
                Q[1]=Q[57].__iter
                ba=ae(-1743)
            end,[31258]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(-15202)
            end,[25343]=function()
                Q[12]=-28909
                ba=ae(18134)
            end,[24331]=function()
                Q[1]=Q[27]==Q[1]
                ba=ae(-3795)
            end,[-6373]=function()
                if not Ab[Q[3][-22190]]then
                    ba=ae(-29412)
                    return true
                end
                ba=ae(5437)
            end,[-31748]=function()
                Q[10]=bb(aa(Q[10],Q[11]))
                ba=ae(-2684)
            end,[13991]=function()
                Q[1]=Q[1]-Q[9]
                ba=ae(21060)
            end,[-4464]=function()
                Q[11]=-22190
                ba=ae(-11530)
            end,[1555]=function()
                Q[1]=Q[1]+Q[9]
                ba=ae(22823)
            end,[1931]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(6584)
            end,[-29568]=function()
                Q[13]=0
                ba=ae(-13845)
            end,[-15690]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(11276)
            end,[-16260]=function()
                Q[8]=Q[1];
                ba=-29704;
            end,[-9471]=function()
                Q[1]=-28909
                ba=-23018
            end,[10803]=function()
                Q[1]=31907
                ba=-6315
            end,[1843]=function()
                Q[1]=Q[27]-Q[1]
                ba=ae(-2670)
            end,[-16294]=function()
                Q[1]=1
                ba=ae(-29768)
            end,[12290]=function()
                Q[9]=Q[3][Q[9]]
                ba=ae(20791)
            end,[31242]=function()
                Q[29]=Q[1];
                if not(Q[29]==0)then
                    ba=ae(-19274)
                    return true
                else
                    ba=ae(4435)
                    return true
                end
                ba=ae(-8628)
            end,[-29915]=function()
                Q[19]..=Q[1];
                ba=ae(24197)
            end,[5596]=function()
                Q[28],Q[58],Q[59]=se(Q[1][1],1,Q[1][2]);
                ba=ae(17526)
            end,[-9594]=function()
                Q[1]=Q[25][Q[43]]
                ba=ae(10483)
            end,[13518]=function()
                Q[60]=se(Q[1][1],1,Q[1][2]);
                ba=ae(-14208);
            end,[10119]=function()
                Q[20][3]=Q[1];
                ba=ae(30622);
            end,[-23325]=function()
                Q[1]=Q[1][Q[9]]
                ba=ae(5374)
            end,[10594]=function()
                Q[24],Q[55],Q[56]=se(Q[1][1],1,Q[1][2]);
                ba=ae(8745)
            end,[4750]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(3049)
            end,[-12232]=function()
                Q[1]=1
                ba=30493
            end,[6246]=function()
                Q[11]=-22190
                ba=-1622
            end,[26746]=function()
                Q[11]=-22190
                ba=ae(-24842)
            end,[23967]=function()
                Q[1]=3800
                ba=-25128
            end,[20254]=function()
                Q[5]=29290
                ba=ae(-28968)
            end,[-16612]=function()
                Q[13]=0
                ba=ae(-17393)
            end,[-25700]=function()
                Q[1]=Q[20][Q[1]]
                ba=ae(33030)
            end,[-29436]=function()
                if Q[3][29290]==156 then
                    ba=ae(28760)
                    return true
                elseif not(Q[3][29290]==219)then
                    ba=ae(-20370)
                    return true
                else
                    ba=ae(26644)
                    return true
                end
                ba=ae(-27077)
            end,[-10659]=function()
                Ab[Q[30]+1]=Q[25];
                ba=-9594;
            end,[23242]=function()
                Q[1]=Q[26][Q[1]]
                ba=ae(18462)
            end,[6178]=function()
                Q[9]=1
                ba=ae(21407)
            end,[-19383]=function()
                Q[9]=135
                ba=5811
            end,[-6472]=function()
                Q[10]=-22190
                ba=11546
            end,[45]=function()
                if Q[29]==2 then
                    ba=ae(-30572)
                    return true
                end
                ba=ae(-26824)
            end,[25913]=function()
                Q[2]-=Q[1];
                ba=ae(-14612);
            end,[-3643]=function()
                Q[11]=-22190
                ba=ae(29956)
            end,[26519]=function()
                Q[1]=Q[45][Q[17]]
                ba=ae(13418)
            end,[-28331]=function()
                Q[45][Q[17]]=Q[42];
                ba=ae(-20394)
            end,[-24812]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(-15658)
            end,[-31117]=function()
                Q[13]=0
                ba=-4298
            end,[30092]=function()
                Q[53][2]=Q[53];
                ba=ae(19490);
            end,[-15821]=function()
                Ab[Q[3][-22190]]=Q[19];
                ba=ae(23102)
            end,[27620]=function()
                Q[1]=Ab[Q[1]]
                ba=ae(-25802)
            end,[-4120]=function()
                Q[2]-=Q[1];
                ba=-27819;
            end,[10134]=function()
                Q[13]=0
                ba=-9318
            end,[9981]=function()
                Q[1]=Qa[Q[1]]
                ba=ae(-25787)
            end,[15960]=function()
                Q[24],Q[55],Q[56]=Q[45];
                if Lc(Q[24])~='function'then
                    ba=ae(-19305)
                    return true
                end;
                ba=ae(-24472);
            end,[-20996]=function()
                Q[1]=Q[31]-Q[1]
                ba=ae(32465)
            end,[-32098]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(19391)
            end,[-10125]=function()
                Q[1]=13017
                ba=ae(-10103)
            end,[7201]=function()
                Q[20][2]=Q[20];
                ba=7450;
            end,[24522]=function()
                Q[1]=bb(Oa(Q[47]))
                ba=ae(13475)
            end,[-572]=function()
                Q[1]=ad[Q[2]]
                ba=ae(-9686)
            end,[3461]=function()
                Q[1]=bb(hc(se(Q[1][1],1,Q[1][2])))
                ba=ae(9365)
            end,[-14150]=function()
                Q[10]=111
                ba=ae(-3794)
            end,[-9318]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(20268)
            end,[22601]=function()
                Ab[Q[3][-22190]]=Q[1];
                ba=ae(23798)
            end,[18644]=function()
                Q[12]=-28909
                ba=ae(-1520)
            end,[16739]=function()
                Q[13]=0
                ba=5167
            end,[-23343]=function()
                if Q[3][29290]==241 then
                    ba=ae(25740)
                    return true
                else
                    ba=ae(-15718)
                    return true
                end
                ba=ae(-23780)
            end,[9553]=function()
                Q[51],Q[6]=Q[1],Q[9];
                ba=ae(-3320);
            end,[-17754]=function()
                Q[12]=-28909
                ba=30897
            end,[9110]=function()
                Q[28],Q[58],Q[59]=Q[61];
                if Lc(Q[28])~='function'then
                    ba=ae(-14263)
                    return true
                end;
                ba=ae(-4386);
            end,[-22615]=function()
                Q[10]=1
                ba=18869
            end,[-17634]=function()
                if not Q[9]then
                    ba=ae(24302)
                    return true
                end
                ba=ae(28313)
            end,[15072]=function()
                Q[1]=1
                ba=ae(5241)
            end,[-25640]=function()
                Q[1]=Q[54]-Q[30]
                ba=ae(14286)
            end,[-17112]=function()
                Q[61][Q[23]]=nil;
                ba=ae(22117)
            end,[3250]=function()
                Q[1]=Q[30]+Q[62]
                ba=-4312
            end,[11135]=function()
                Q[9]=250
                ba=6886
            end,[14210]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(19899)
            end,[-31400]=function()
                Ab[Q[3][-22190]]=Q[37];
                ba=ae(-5447)
            end,[2793]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(7144)
            end,[-501]=function()
                Q[1]=bb(Wb(se(Q[1][1],1,Q[1][2])))
                ba=ae(10509)
            end,[-27853]=function()
                Q[1]=28498
                ba=ae(20717)
            end,[-6145]=function()
                Q[1]=1
                ba=ae(21457)
            end,[-29351]=function()
                Q[44]=Q[1];
                ba=ae(-20218)
            end,[-2577]=function()
                Q[11]=Q[3][Q[11]]
                ba=18644
            end,[11031]=function()
                Q[47],Q[48],Q[49]=Q[45];
                if Lc(Q[47])~='function'then
                    ba=ae(-10482)
                    return true
                end;
                ba=ae(3158);
            end,[-27453]=function()
                Q[1]=-22190
                ba=ae(-8640)
            end,[25893]=function()
                Q[1]=-22190
                ba=ae(-31891)
            end,[-28613]=function()
                Q[27],Q[31]=Q[1],Q[9];
                ba=ae(-13494);
            end,[-4939]=function()
                Q[1]=Q[60][Q[1]]
                ba=ae(-8166)
            end,[-4589]=function()
                Q[10]=-22190
                ba=-21490
            end,[-28625]=function()
                Q[1]=te[Q[1]]
                ba=ae(5207)
            end,[-17072]=function()
                Q[1]=1
                ba=ae(-12531)
            end,[-7893]=function()
                Q[9]=1
                ba=ae(-21751)
            end,[-25006]=function()
                Q[1]=2
                ba=ae(32941)
            end,[-2229]=function()
                Q[1]=Q[1]+Q[9]
                ba=-28625
            end,[-7585]=function()
                Q[9]=Ab[Q[30]]
                ba=ae(26649)
            end,[-23719]=function()
                Q[12]=-28909
                ba=ae(16699)
            end,[20492]=function()
                Q[9]=3
                ba=ae(14349)
            end,[20243]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(-3033)
            end,[-2836]=function()
                Q[52]=se(Q[1][1],1,Q[1][2]);
                if Q[52]~=nil and Q[52].__iter~=nil then
                    ba=ae(-12518)
                    return true
                elseif Lc(Q[47])=='table'then
                    ba=ae(-21599)
                    return true
                end
                ba=ae(-17314)
            end,[4683]=function()
                Q[1]=2
                ba=-5435
            end,[-12909]=function()
                Q[23]=Q[16];
                if Q[14]~=Q[14]then
                    ba=ae(16880)
                else
                    ba=ae(23197)
                end
            end,[-32481]=function()
                Q[10]=-22190
                ba=ae(3798)
            end,[24420]=function()
                Q[1]=Q[1]-Q[9]
                ba=ae(-12898)
            end,[7450]=function()
                Q[1]=1
                ba=ae(12711)
            end,[-2286]=function()
                Q[9]=1
                ba=ae(32694)
            end,[-12951]=function()
                Q[39]=Q[39]+Q[38];
                Q[23]=Q[39];
                if Q[39]~=Q[39]then
                    ba=ae(11649)
                else
                    ba=ae(543)
                end
            end,[-14244]=function()
                Ab[Q[3][-22190]]=Q[1];
                ba=ae(16874);
            end,[7582]=function()
                Q[4]=Q[3][Q[4]]
                ba=ae(5300)
            end,[21571]=function()
                Q[10]=Q[3][Q[10]]
                ba=17070
            end,[7474]=function()
                Q[54]=Q[1];
                ba=ae(19646)
            end,[4796]=function()
                Q[1]=21753
                ba=ae(29193)
            end,[19588]=function()
                Q[1]=1
                ba=-7969
            end,[-24287]=function()
                Q[1]=-32174
                ba=-29622
            end,[26288]=function()
                Q[4]=-28909
                ba=ae(-31451)
            end,[28691]=function()
                Q[9]=Q[3][Q[9]]
                ba=ae(-8385)
            end,[-16441]=function()
                Q[22][(Q[23]-161)]=Q[1];
                ba=ae(-8701)
            end,[-32561]=function()
                Q[9]=Q[3][Q[9]]
                ba=ae(20942)
            end,[-3616]=function()
                Ab[Q[3][-22190]]=Q[1];
                ba=ae(-16506)
            end,[32295]=function()
                Q[1]=Q[27]-Q[1]
                ba=ae(5713)
            end,[-27173]=function()
                Q[2]-=Q[1];
                ba=ae(-13937);
            end,[23307]=function()
                Q[1]=1
                ba=ae(26544)
            end,[2287]=function()
                Q[1]=te[Q[1]]
                ba=ae(-9949)
            end,[26073]=function()
                Q[2]+=Q[1];
                ba=ae(32641)
            end,[20941]=function()
                Q[1]=-10759
                ba=-8209
            end,[26522]=function()
                Q[9]=Q[11]
                ba=ae(-8719)
            end,[31033]=function()
                Q[53]=Q[1];
                ba=ae(-13632);
            end,[-8861]=function()
                Q[1]=-28909
                ba=-26615
            end,[-355]=function()
                Q[9]=91
                ba=-4589
            end,[-25347]=function()
                Q[4]=Q[3][Q[4]]
                ba=ae(-30592)
            end,[28489]=function()
                Ab[Q[3][-22190]]=nil;
                ba=ae(-14044)
            end,[26990]=function()
                Q[1]=1
                ba=ae(3176)
            end,[21823]=function()
                Q[1]=-22190
                ba=-15690
            end,[-11933]=function()
                Q[1]=-22190
                ba=10002
            end,[-12496]=function()
                Q[1]=bb(Pb(Ab,Q[30],Q[1]))
                ba=ae(27583)
            end,[20992]=function()
                Q[11]=-22190
                ba=-2577
            end,[12646]=function()
                Q[9]=Q[3][Q[9]]
                ba=ae(-17205)
            end,[-18039]=function()
                Q[1]=Q[20][Q[1]]
                ba=27932
            end,[-9743]=function()
                Q[9]=Q[1]
                ba=30039
            end,[2847]=function()
                Q[9]=-28909
                ba=8482
            end,[2372]=function()
                Q[1]=bb(Oa(Q[24]))
                ba=ae(10338)
            end,[8038]=function()
                Q[1]=bb((function(sc,x)
                    local N,Va,jc,za,ne
                    za={}
                    ne,jc={[-31361]=-25499,[16186]=-10674,[-23162]=23850,[24610]=-8955,[-2817]=-15120,[25630]=-32295,[-8188]=15260,[19659]=-23998,[22878]=-18516,[-10684]=-22869,[14188]=-10635,[-9199]=15087,[-6919]=-9994,[908]=-14416,[23454]=29194,[-20764]=-23998,[-27296]=-23998,[4201]=450,[-16228]=-8461,[11482]=-18731,[7471]=-2687,[-1291]=-23998,[26704]=-23998,[-27182]=450,[-4692]=29194,[-20314]=-10674,[10644]=-23824},function(C)
                        return ne[C+-24366]
                    end
                    Va={[-15120]=function()
                        za[1]=(za[1])%za[2]
                        N=jc(38554)
                    end,[-22869]=function()
                        if(za[3]>=0 and za[4]>za[5])or((za[3]<0 or za[3]~=za[3])and za[4]<za[5])then
                            N=jc(23075)
                        else
                            N=jc(25274)
                        end
                    end,[-14416]=function()
                        za[6]=10
                        N=jc(1204)
                    end,[-9854]=function()
                        za[7]=za[4];
                        if za[5]~=za[5]then
                            N=jc(-2930)
                        else
                            N=jc(13682)
                        end
                    end,[-2687]=function()
                        za[2]=#x
                        N=jc(21549)
                    end,[-32295]=function()
                        za[6]=za[8]..se(za[6][1],1,za[6][2])
                        N=jc(40552)
                    end,[-6962]=function()
                        za[6]=bb(aa(za[6],se(za[1][1],1,za[1][2])))
                        N=11972
                    end,[-10674]=function()
                        za[8]=za[6];
                        N=jc(47244)
                    end,[729]=function()
                        za[1]=10
                        N=28188
                    end,[-18731]=function()
                        za[1]=bb(ac(x,za[1]))
                        N=-6962
                    end,[450]=function()
                        za[6]=''
                        N=jc(19674)
                    end,[11972]=function()
                        za[6]=bb(zd(se(za[6][1],1,za[6][2])))
                        N=jc(49996)
                    end,[-10635]=function()
                        za[2]=1
                        N=-31606
                    end,[-18516]=function()
                        za[4]=za[4]+za[3];
                        za[7]=za[4];
                        if za[4]~=za[4]then
                            N=jc(51070)
                        else
                            N=-22869
                        end
                    end,[-2078]=function()
                        za[6]=(za[6])+za[1]
                        N=jc(8138)
                    end,[-8461]=function()
                        za[6]=ac(sc,za[6])
                        N=729
                    end,[3077]=function()
                        za[1]=1
                        N=-2078
                    end,[23850]=function()
                        za[6]=za[7]-za[6]
                        N=3077
                    end,[28188]=function()
                        za[1]=za[7]-za[1]
                        N=jc(31837)
                    end,[-31606]=function()
                        za[1]=za[1]+za[2]
                        N=jc(35848)
                    end}
                    N=jc(28567)
                    repeat
                        while true do
                            za[9]=Va[N]
                            if za[9]~=nil then
                                if za[9]()then
                                    break
                                end
                            elseif N==29194 then
                                za[8]=za[6];
                                za[3],za[4],za[5]=1,10,(#sc-1)+10
                                N=-9854
                            elseif N==-23998 then
                                return za[8]
                            end
                        end
                    until N==15139
                end)(Q[1],Q[9]))
                ba=ae(11135)
            end,[-25072]=function()
                Q[10]=Q[3][Q[10]]
                ba=ae(25842)
            end,[-4836]=function()
                Q[53][2]=Q[53];
                ba=ae(-22040);
            end,[-31003]=function()
                Q[1]=Q[9]
                ba=ae(-18999)
            end,[-1018]=function()
                Q[9]=ad[Q[9]]
                ba=ae(19241)
            end,[-16819]=function()
                Q[1]=ec(Q[1],Q[9],Q[41],Q[30],Ab)
                ba=ae(20375)
            end,[2314]=function()
                Q[47],Q[48],Q[49]=se(Q[1][1],1,Q[1][2]);
                ba=ae(14147)
            end,[-18193]=function()
                Q[1]=Q[1]+Q[9]
                ba=ae(15377)
            end,[-1383]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(11333)
            end,[-12376]=function()
                Q[9]=Q[20][Q[9]]
                ba=-23325
            end,[-8749]=function()
                Q[1]=ec(Q[1],Q[9],Q[62],Q[30],Ab)
                ba=ae(16906)
            end,[6261]=function()
                Q[9]=174
                ba=-24788
            end,[-12579]=function()
                Q[1]=_a[Q[1]]
                ba=ae(3569)
            end,[-1024]=function()
                if not(Q[3][29290]==45)then
                    ba=ae(22691)
                    return true
                else
                    ba=ae(-8486)
                    return true
                end
                ba=ae(-7653)
            end,[5968]=function()
                Q[1]=Q[1]+Q[9]
                ba=2287
            end,[-15938]=function()
                Q[51],Q[7]=Q[1],Q[9];
                ba=ae(20076);
            end,[19856]=function()
                Q[9]=3
                ba=-12376
            end,[1602]=function()
                Q[11]=1
                ba=-13930
            end,[2982]=function()
                Q[9]=-28909
                ba=ae(-7006)
            end,[-6999]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(12077)
            end,[-22397]=function()
                Q[1]=j(Q[63])
                ba=ae(4450)
            end,[-16927]=function()
                Q[1]=28498
                ba=ae(29748)
            end,[-9056]=function()
                if not(Q[20][3]>=Q[3][-22190])then
                    ba=ae(-5096)
                    return true
                else
                    ba=ae(29689)
                    return true
                end
                ba=ae(18940)
            end,[-20363]=function()
                Q[9]=1
                ba=ae(-15028)
            end,[-18849]=function()
                Q[9]=Ab[Q[9]]
                ba=ae(-12336)
            end,[-27842]=function()
                ad[Q[2]]=Q[1];
                ba=ae(-1118)
            end,[-4972]=function()
                Q[2]+=Q[1];
                ba=ae(-2653)
            end,[21048]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(-8094)
            end,[8107]=function()
                Q[26]=Q[1];
                ba=-14509;
            end,[5923]=function()
                Q[9]=15917
                ba=ae(-5745)
            end,[-4439]=function()
                Q[9]=Q[3][Q[9]]
                ba=ae(-28905)
            end,[2766]=function()
                Q[4]=Q[3][Q[4]]
                ba=ae(-29706)
            end,[28604]=function()
                Q[1]=bb(lc(Q[47]))
                ba=ae(-13974)
            end,[-7461]=function()
                Q[1]=Q[1]-Q[9]
                ba=-12496
            end,[-5429]=function()
                Q[22][(Q[23]-151)]=Q[53];
                ba=ae(-8215)
            end,[31297]=function()
                Q[11]=Q[3][Q[11]]
                ba=ae(-8868)
            end,[19080]=function()
                Q[4]=Q[3][Q[4]]
                ba=ae(-3174)
            end,[15211]=function()
                Q[1]=Q[6][Q[1]]
                ba=ae(-10819)
            end,[-11243]=function()
                Q[1]=1
                ba=ae(5381)
            end,[22602]=function()
                Q[2]-=Q[1];
                ba=7023;
            end,[-1799]=function()
                Q[17]=Q[1];
                ba=-18609;
            end,[-4373]=function()
                Q[1]=Q[3][Q[1]]
                ba=-25161
            end,[16441]=function()
                Q[11]=Q[10]
                ba=19493
            end,[2942]=function()
                Q[1]=28498
                ba=-19383
            end,[-6954]=function()
                Q[10]=4049
                ba=ae(-19561)
            end,[29640]=function()
                Q[20][1]=Q[1];
                ba=ae(20576);
            end,[7936]=function()
                Ab[Q[3][-28909]][Q[17]]=Q[1];
                ba=23307;
            end,[11707]=function()
                Q[28],Q[58],Q[59]=se(Q[1][1],1,Q[1][2]);
                ba=ae(-31058)
            end,[-25676]=function()
                Q[1]=Q[37][Q[1]]
                ba=ae(26261)
            end,[21579]=function()
                Q[1]=3800
                ba=ae(-1442)
            end,[-4294]=function()
                Q[23],Q[20]=Q[24](Q[55],Q[56]);
                Q[56]=Q[23];
                if Q[56]==nil then
                    ba=ae(-29865)
                else
                    ba=ae(-24221)
                end
            end,[-32202]=function()
                Q[1]=28498
                ba=-16979
            end,[-18609]=function()
                Q[1]=-28909
                ba=-11165
            end,[-26615]=function()
                Q[1]=Q[21][Q[1]]
                ba=ae(-23015)
            end,[-28489]=function()
                Q[12]=-28909
                ba=-6941
            end,[-9379]=function()
                Q[11]=Q[3][Q[11]]
                ba=ae(15568)
            end,[27932]=function()
                Q[9]=3
                ba=-7892
            end,[-19601]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(19631)
            end,[18869]=function()
                Q[10]=Q[27]-Q[10]
                ba=24322
            end,[26078]=function()
                Q[9]=Q[20][Q[9]]
                ba=ae(5409)
            end,[-8423]=function()
                Q[11]=1
                ba=ae(-3638)
            end,[-28144]=function()
                Q[1]=Q[21][Q[1]]
                ba=-2286
            end,[7520]=function()
                Q[1]=28498
                ba=-13863
            end,[26118]=function()
                Q[5]=29290
                ba=ae(13722)
            end,[27894]=function()
                Q[1]=-10759
                ba=2793
            end,[30359]=function()
                Q[2]-=Q[1];
                ba=15416;
            end,[-25680]=function()
                Q[1]=-10759
                ba=1931
            end,[9769]=function()
                Q[10]=1
                ba=13004
            end,[15343]=function()
                Q[11]={__mode=Q[11]}
                ba=ae(19220)
            end,[-4171]=function()
                Q[9]=Q[30]+Q[32]
                ba=-16565
            end,[-26033]=function()
                Q[2]+=Q[1];
                ba=ae(-163)
            end,[-17327]=function()
                Q[1]=Q[1][Q[9]]
                ba=ae(9333)
            end,[27831]=function()
                Q[1]=1
                ba=ae(-15047)
            end,[29555]=function()
                if not(Q[44]==216)then
                    ba=ae(4915)
                    return true
                else
                    ba=ae(-31178)
                    return true
                end
                ba=ae(-8422)
            end,[15914]=function()
                Q[57]=se(Q[1][1],1,Q[1][2]);
                if Q[57]~=nil and Q[57].__iter~=nil then
                    ba=ae(22030)
                    return true
                elseif Lc(Q[28])=='table'then
                    ba=ae(-9970)
                    return true
                end
                ba=ae(-31321)
            end,[28290]=function()
                Q[10]={}
                ba=32282
            end,[31341]=function()
                Q[1]=Q[30]+Q[41]
                ba=-17922
            end,[12790]=function()
                Q[1]=-28909
                ba=ae(26074)
            end,[9303]=function()
                Q[1]=28498
                ba=ae(7002)
            end,[-25256]=function()
                Q[4]=-28909
                ba=19080
            end,[24322]=function()
                Q[11]=Q[10]
                ba=ae(-20902)
            end,[19533]=function()
                Q[1]=1
                ba=ae(-4212)
            end,[-84]=function()
                Q[1]=bb(Q[1](Q[47]))
                ba=ae(-13400)
            end,[27708]=function()
                Q[5]=29290
                ba=17934
            end,[5696]=function()
                Q[2]+=Q[1];
                ba=ae(5511)
            end,[8326]=function()
                Q[1]=Q[1][Q[9]]
                ba=ae(-17346)
            end,[17002]=function()
                ad[Q[2]]=Q[1];
                ba=ae(-21011)
            end,[17629]=function()
                Q[9]=2
                ba=-6872
            end,[-17935]=function()
                Q[1]={[Q[1]]=Q[9],[Q[10]]=Q[11],[Q[12]]=Q[4],[Q[5]]=Q[13]}
                ba=ae(-12478)
            end,[16899]=function()
                Q[9]=134
                ba=13409
            end,[29023]=function()
                Q[64]=Q[1];
                ba=ae(18478)
            end,[-21199]=function()
                Q[2]-=Q[1];
                ba=9303;
            end,[14614]=function()
                if true then
                    ba=ae(24354)
                else
                    ba=ae(-22606)
                end
            end,[-12766]=function()
                Ab[Q[3][-22190]]=Q[1];
                ba=ae(32582)
            end,[17070]=function()
                Q[11]=189
                ba=-31748
            end,[-32723]=function()
                Q[64]=Q[1];
                ba=ae(1967)
            end,[13019]=function()
                Q[1]=Ab[Q[1]]
                ba=ae(-9908)
            end,[-24576]=function()
                Q[11]=cc(Q[11],Q[12])
                ba=ae(31446)
            end,[31380]=function()
                Q[2]-=Q[1];
                ba=-27853;
            end,[-851]=function()
                Q[30],Q[41]=Q[1],Q[9];
                if not(Q[41]==Qb)then
                    ba=ae(-13196)
                    return true
                else
                    ba=ae(-12574)
                    return true
                end;
                ba=ae(8410);
            end,[29313]=function()
                Q[2]+=Q[1];
                ba=ae(-2647)
            end,[-992]=function()
                Q[9]=29290
                ba=12646
            end,[-27331]=function()
                Q[1]=Q[30]+Q[64]
                ba=-7893
            end,[27021]=function()
                Q[4]=Q[3][Q[4]]
                ba=-19454
            end,[-15248]=function()
                if not(Q[44]==125)then
                    ba=ae(-12687)
                    return true
                else
                    ba=ae(-23205)
                    return true
                end
                ba=ae(3880)
            end,[7842]=function()
                Q[1]=1
                ba=ae(20463)
            end,[13366]=function()
                Q[10]=-22190
                ba=ae(-11610)
            end,[-26880]=function()
                Q[1]=2
                ba=-18039
            end,[-14823]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(5106)
            end,[24340]=function()
                if Ab[Q[3][-22190]]then
                    ba=ae(-28403)
                    return true
                end
                ba=ae(12417)
            end,[-5074]=function()
                Q[1]=-28909
                ba=ae(-17047)
            end,[1305]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(-12433)
            end,[-14509]=function()
                Q[1]=22867
                ba=ae(-1081)
            end,[-30845]=function()
                if Q[9]then
                    ba=ae(4914)
                    return true
                end
                ba=-22615
            end,[14780]=function()
                Q[1]=1
                ba=ae(32562)
            end,[-31378]=function()
                Q[1]=bb(Oa(Q[28]))
                ba=ae(7557)
            end,[-22316]=function()
                Q[11]=-22190
                ba=-14203
            end,[20519]=function()
                if not(Q[3][29290]==140)then
                    ba=ae(-29435)
                    return true
                else
                    ba=ae(22204)
                    return true
                end
                ba=ae(-13718)
            end,[16723]=function()
                Q[1]=bb(lc(Q[28]))
                ba=ae(-3536)
            end,[-16565]=function()
                Q[1]=bb(Pb(Ab,Q[1],Q[9]))
                ba=-6875
            end,[29274]=function()
                Ab[Q[3][-22190]]=Q[1];
                ba=ae(-8274)
            end,[-11165]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(-19360)
            end,[-13532]=function()
                Q[22][(Q[23]-161)]=Q[53];
                ba=ae(11101)
            end,[-6739]=function()
                Q[26]=Q[1];
                ba=ae(-13437);
            end,[-8132]=function()
                Q[12]=-28909
                ba=26288
            end,[-11881]=function()
                Q[13]=0
                ba=31258
            end,[-24325]=function()
                Q[1]=Ab[Q[27]]
                ba=ae(4574)
            end,[9573]=function()
                ad[Q[2]]=Q[1];
                ba=ae(-9215)
            end,[2966]=function()
                Q[42]=Q[1];
                ba=ae(-2191);
            end,[-31559]=function()
                Q[17]=Q[1];
                ba=21823;
            end,[32282]=function()
                Q[11]='vs'
                ba=ae(-27288)
            end,[16103]=function()
                Q[47],Q[48],Q[49]=se(Q[1][1],1,Q[1][2]);
                ba=ae(26244)
            end,[30493]=function()
                Q[1]=Q[30]+Q[1]
                ba=ae(16468)
            end,[-1941]=function()
                ad[Q[2]]=Q[1];
                ba=ae(-7666)
            end,[-1224]=function()
                Q[1]=1
                ba=ae(6363)
            end,[-6875]=function()
                Q[1]=bb(Q[33](se(Q[1][1],1,Q[1][2])))
                ba=-501
            end,[-25128]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(32487)
            end,[878]=function()
                Q[13]=0
                ba=ae(-30648)
            end,[-7180]=function()
                Q[5]=29290
                ba=ae(1531)
            end,[31683]=function()
                Q[5]=29290
                ba=ae(19598)
            end,[30954]=function()
                Q[1]=1
                ba=ae(-15054)
            end,[488]=function()
                Q[1]=Q[1]+Q[9]
                ba=-20598
            end,[-28733]=function()
                Q[11]={}
                ba=ae(27346)
            end,[-32132]=function()
                Q[13]=0
                ba=ae(3517)
            end,[-30195]=function()
                Q[6][-10759]=se(Q[1][1],1,Q[1][2]);
                if Q[51]==2 then
                    ba=ae(-25121)
                    return true
                elseif Q[51]==3 then
                    ba=ae(-2588)
                    return true
                end
                ba=ae(112)
            end,[-29622]=function()
                Q[1]=Q[60][Q[1]]
                ba=ae(-28667)
            end,[889]=function()
                Q[12]=-28909
                ba=ae(5016)
            end,[10002]=function()
                Q[1]=Q[21][Q[1]]
                ba=ae(-31558)
            end,[-252]=function()
                Q[1]=Q[26][Q[1]]
                ba=ae(3879)
            end,[13278]=function()
                Q[6][15917]=se(Q[1][1],1,Q[1][2]);
                ba=ae(553)
            end,[-2377]=function()
                Q[1]=1
                ba=ae(-23524)
            end,[10562]=function()
                Q[1]=bb((function(qb,Ka)
                    local Id,pc,Vb,Sa,ya
                    Id={}
                    ya,Vb={[24823]=-16019,[23351]=-14841,[-26449]=-13607,[-18187]=23642,[-12617]=-17251,[8974]=-17251,[-19967]=8788,[-16549]=-1061,[7806]=30935,[2788]=-25987,[31073]=-9966,[31223]=-27995,[21439]=-23664,[2299]=-21475,[-20275]=-17251,[7100]=-8908,[-10739]=-21475,[-23746]=-17251,[19303]=-1894,[-6678]=19450,[-13636]=31956,[1934]=-17251,[21791]=1904,[32632]=18945,[-8929]=1904,[-7655]=16366},function(Jb)
                        return ya[Jb+19949]
                    end
                    Sa={[31956]=function()
                        Id[1]=(Id[1])+Id[2]
                        pc=-14647
                    end,[8788]=function()
                        Id[1]=Id[3]-Id[1]
                        pc=-629
                    end,[-10961]=function()
                        Id[2]=Id[2]+Id[4]
                        pc=-28205
                    end,[-9966]=function()
                        Id[2]=65
                        pc=Vb(-38136)
                    end,[-28205]=function()
                        Id[2]=bb(ac(Ka,Id[2]))
                        pc=-7332
                    end,[-27995]=function()
                        Id[1]=bb(zd(se(Id[1][1],1,Id[1][2])))
                        pc=Vb(12683)
                    end,[18945]=function()
                        Id[1]=Id[5]..se(Id[1][1],1,Id[1][2])
                        pc=Vb(-17650)
                    end,[23642]=function()
                        Id[2]=Id[3]-Id[2]
                        pc=Vb(-12849)
                    end,[19450]=function()
                        Id[2]=(Id[2])%Id[4]
                        pc=Vb(4874)
                    end,[-7332]=function()
                        Id[1]=bb(aa(Id[1],se(Id[2][1],1,Id[2][2])))
                        pc=Vb(11274)
                    end,[-23664]=function()
                        Id[3]=Id[6];
                        if Id[7]~=Id[7]then
                            pc=Vb(-40224)
                        else
                            pc=10975
                        end
                    end,[-25987]=function()
                        Id[1]=''
                        pc=Vb(-28878)
                    end,[-16019]=function()
                        Id[4]=1
                        pc=-10961
                    end,[-14647]=function()
                        Id[1]=ac(qb,Id[1])
                        pc=Vb(11124)
                    end,[-14841]=function()
                        Id[6]=Id[6]+Id[8];
                        Id[3]=Id[6];
                        if Id[6]~=Id[6]then
                            pc=Vb(-43695)
                        else
                            pc=10975
                        end
                    end,[-13607]=function()
                        Id[1]=65
                        pc=Vb(-39916)
                    end,[-629]=function()
                        Id[2]=1
                        pc=Vb(-33585)
                    end,[10975]=function()
                        if(Id[8]>=0 and Id[6]>Id[7])or((Id[8]<0 or Id[8]~=Id[8])and Id[6]<Id[7])then
                            pc=Vb(-32566)
                        else
                            pc=Vb(-46398)
                        end
                    end,[-21475]=function()
                        Id[5]=Id[1];
                        pc=Vb(3402)
                    end,[-8908]=function()
                        Id[4]=#Ka
                        pc=Vb(-26627)
                    end}
                    pc=Vb(-17161)
                    repeat
                        while true do
                            Id[9]=Sa[pc]
                            if Id[9]~=nil then
                                if Id[9]()then
                                    break
                                end
                            elseif pc==-17251 then
                                return Id[5]
                            elseif pc==1904 then
                                Id[5]=Id[1];
                                Id[8],Id[6],Id[7]=1,65,(#qb-1)+65
                                pc=Vb(1490)
                            end
                        end
                    until pc==-2109
                end)(Q[1],Q[9]))
                ba=ae(-2371)
            end,[31263]=function()
                Q[5]=29290
                ba=-16612
            end,[5328]=function()
                Q[11]=-22190
                ba=ae(-17532)
            end,[14996]=function()
                Q[9]=111
                ba=ae(-12114)
            end,[-21971]=function()
                Q[4]=-28909
                ba=ae(6619)
            end,[-32203]=function()
                Q[2]-=Q[1];
                ba=-29240;
            end,[28433]=function()
                Q[1]=-10759
                ba=ae(-27207)
            end,[32670]=function()
                Q[2]+=Q[1];
                ba=ae(3521)
            end,[-5220]=function()
                if(Q[35]>=0 and Q[36]>Q[34])or((Q[35]<0 or Q[35]~=Q[35])and Q[36]<Q[34])then
                    ba=ae(3498)
                else
                    ba=ae(-24218)
                end
            end,[31172]=function()
                if not(Q[3][29290]==102)then
                    ba=ae(-3354)
                    return true
                else
                    ba=ae(-27934)
                    return true
                end
                ba=ae(-19567)
            end,[14124]=function()
                Q[23]=Q[39];
                if Q[40]~=Q[40]then
                    ba=ae(-8046)
                else
                    ba=-20063
                end
            end,[533]=function()
                Q[1]=1
                ba=ae(32021)
            end,[31309]=function()
                Q[9]=-10759
                ba=-4439
            end,[-17276]=function()
                Q[54]=Q[1];
                ba=ae(-4205)
            end,[26959]=function()
                Q[1]=''
                ba=ae(-23407)
            end,[-23536]=function()
                Q[5]=29290
                ba=-31117
            end,[-26348]=function()
                Q[21]=Q[1];
                ba=15072;
            end,[-8510]=function()
                Q[1]=15704
                ba=5151
            end,[1312]=function()
                Q[1]=bb(Q[1](Q[24]))
                ba=ae(12453)
            end,[11798]=function()
                Q[22]=se(Q[1][1],1,Q[1][2]);
                ba=ae(-16176);
            end,[-1622]=function()
                Q[11]=Q[3][Q[11]]
                ba=ae(-24949)
            end,[-29191]=function()
                Q[4]=-28909
                ba=32157
            end,[-18472]=function()
                if Q[3][29290]==152 then
                    ba=ae(-16923)
                    return true
                else
                    ba=ae(-28770)
                    return true
                end
                ba=ae(18904)
            end,[3158]=function()
                Q[2]+=Q[1];
                ba=ae(28282)
            end,[-19454]=function()
                Q[5]=29290
                ba=16739
            end,[29885]=function()
                Q[10]=29290
                ba=ae(29418)
            end,[237]=function()
                Q[22]=se(Q[1][1],1,Q[1][2]);
                ba=ae(-28097);
            end,[3033]=function()
                Q[11]=-22190
                ba=ae(-30816)
            end,[23455]=function()
                Q[1]=_a[Q[1]]
                ba=29188
            end,[-24703]=function()
                Q[9]=15917
                ba=ae(18609)
            end,[8255]=function()
                Q[1]=bb((function(Hd,Yc)
                    local be,qc,n,Vc,Ub
                    n={}
                    be,Ub={[-21874]=7857,[25652]=-3367,[25301]=14945,[20293]=23446,[9039]=29520,[-10548]=23446,[4547]=7857,[29466]=-26071,[2021]=14351,[-20445]=6378,[10054]=6357,[-20270]=-6653,[25519]=14639,[-22269]=23446,[-16074]=5954,[-26151]=-19510,[17238]=14103,[28623]=-10114,[-20702]=23446,[-7445]=-3367,[11808]=23446,[-17180]=-4721,[5703]=-12897,[-15216]=-23578,[14361]=14351,[3929]=27806,[-2386]=14103},function(b)
                        return be[b-5753]
                    end
                    Vc={[-12897]=function()
                        n[1]=bb(zd(se(n[1][1],1,n[1][2])))
                        qc=-7235
                    end,[27806]=function()
                        n[2]=bb(ac(Yc,n[2]))
                        qc=28010
                    end,[-7235]=function()
                        n[1]=n[3]..se(n[1][1],1,n[1][2])
                        qc=Ub(10300)
                    end,[14639]=function()
                        n[1]=n[4]-n[1]
                        qc=Ub(-20398)
                    end,[3546]=function()
                        n[2]=(n[2])%n[5]
                        qc=Ub(15807)
                    end,[6002]=function()
                        n[2]=n[2]+n[5]
                        qc=Ub(9682)
                    end,[-24619]=function()
                        n[5]=#Yc
                        qc=3546
                    end,[-23578]=function()
                        n[2]=122
                        qc=Ub(31054)
                    end,[14945]=function()
                        n[2]=n[4]-n[2]
                        qc=-24619
                    end,[29520]=function()
                        n[1]=122
                        qc=Ub(31272)
                    end,[-19510]=function()
                        n[2]=1
                        qc=Ub(-14517)
                    end,[-4721]=function()
                        if(n[6]>=0 and n[7]>n[8])or((n[6]<0 or n[6]~=n[6])and n[7]<n[8])then
                            qc=Ub(-14949)
                        else
                            qc=Ub(14792)
                        end
                    end,[28010]=function()
                        n[1]=bb(aa(n[1],se(n[2][1],1,n[2][2])))
                        qc=Ub(11456)
                    end,[7857]=function()
                        n[3]=n[1];
                        qc=Ub(34376)
                    end,[29212]=function()
                        n[1]=ac(Hd,n[1])
                        qc=Ub(-9463)
                    end,[14103]=function()
                        n[1]=''
                        qc=Ub(-1692)
                    end,[5954]=function()
                        n[4]=n[7];
                        if n[8]~=n[8]then
                            qc=Ub(-4795)
                        else
                            qc=Ub(-11427)
                        end
                    end,[-6653]=function()
                        n[1]=(n[1])+n[2]
                        qc=29212
                    end,[6357]=function()
                        n[5]=1
                        qc=6002
                    end,[-10114]=function()
                        n[7]=n[7]+n[6];
                        n[4]=n[7];
                        if n[7]~=n[7]then
                            qc=Ub(26046)
                        else
                            qc=-4721
                        end
                    end}
                    qc=Ub(22991)
                    repeat
                        while true do
                            n[9]=Vc[qc]
                            if n[9]~=nil then
                                if n[9]()then
                                    break
                                end
                            elseif qc==-3367 then
                                n[3]=n[1];
                                n[8],n[6],n[7]=(#Hd-1)+122,1,122
                                qc=Ub(-10321)
                            elseif qc==23446 then
                                return n[3]
                            end
                        end
                    until qc==-23874
                end)(Q[1],Q[9]))
                ba=ae(-27552)
            end,[-22561]=function()
                Q[9]=1
                ba=ae(32879)
            end,[-14203]=function()
                Q[11]=Q[3][Q[11]]
                ba=ae(-25468)
            end,[-13841]=function()
                Q[9]=aa(Q[9],Q[10])
                ba=ae(-12303)
            end,[19872]=function()
                Ab[aa(Q[3][-22190],116)]=se(Q[1][1],1,Q[1][2]);
                Q[39],Q[40],Q[38]=162,(Q[8])+161,1
                ba=14124
            end,[-4683]=function()
                ad[Q[2]]=Q[1];
                ba=ae(18111)
            end,[13409]=function()
                Q[10]=-22190
                ba=3033
            end,[-5435]=function()
                Q[1]=Q[20][Q[1]]
                ba=19856
            end,[6198]=function()
                Q[4]=-28909
                ba=ae(6896)
            end,[29188]=function()
                Q[9]=1
                ba=-16819
            end,[18824]=function()
                Q[1]=22867
                ba=23242
            end,[27865]=function()
                Q[9]=Q[1]
                ba=ae(-25819)
            end,[-22719]=function()
                ad[Q[2]]=Q[1];
                ba=ae(-20150)
            end,[-10309]=function()
                Q[1]=1
                ba=ae(24677)
            end,[-10044]=function()
                Q[11]=Q[3][Q[11]]
                ba=10249
            end,[27196]=function()
                Q[13]=0
                ba=ae(3598)
            end,[-6225]=function()
                Q[20]=Q[1];
                ba=-26880;
            end,[-24089]=function()
                Ab[Q[3][-22190]]=Q[1];
                ba=ae(9276)
            end,[-8642]=function()
                Q[9]=109
                ba=ae(23114)
            end,[8634]=function()
                Q[23]=Q[36];
                if Q[34]~=Q[34]then
                    ba=ae(11506)
                else
                    ba=-5220
                end
            end,[-22038]=function()
                Q[37]=Q[1];
                if Q[51]==1 then
                    ba=ae(882)
                    return true
                elseif not(Q[51]==2)then
                    ba=ae(-6857)
                    return true
                else
                    ba=ae(-4671)
                    return true
                end
                ba=ae(29368)
            end,[-26981]=function()
                Q[4]=Q[3][Q[4]]
                ba=31683
            end,[9704]=function()
                Q[1]=-10759
                ba=-28931
            end,[-9595]=function()
                Q[11]=Q[3][Q[11]]
                ba=-2338
            end,[26136]=function()
                Q[1]=Q[9]
                ba=ae(27446)
            end,[-31288]=function()
                Q[23],Q[63]=Q[28](Q[58],Q[59]);
                Q[59]=Q[23];
                if Q[59]==nil then
                    ba=-31463
                else
                    ba=ae(-27503)
                end
            end,[22513]=function()
                Q[3]=Q[1];
                ba=-16927;
            end,[10732]=function()
                Q[9]=Q[3][Q[9]]
                ba=28848
            end,[28669]=function()
                Q[3][28498]=Q[1];
                ba=ae(-6845)
            end,[-22117]=function()
                Q[1]=28498
                ba=14996
            end,[31973]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(24803)
            end,[-29221]=function()
                Q[9]=Q[21][Q[9]]
                ba=-15900
            end,[-15113]=function()
                Q[13]=0
                ba=-15845
            end,[-7536]=function()
                Q[9]=1
                ba=ae(30490)
            end,[-2410]=function()
                Q[9]=-28909
                ba=28691
            end,[-5583]=function()
                Q[9]=1
                ba=29523
            end,[-11564]=function()
                Q[1]=Q[1][Q[9]]
                ba=ae(1752)
            end,[12018]=function()
                Q[10]=-22190
                ba=28510
            end,[-17922]=function()
                Q[9]=1
                ba=24420
            end,[-27745]=function()
                Q[1]=Q[50]+Q[1]
                ba=9981
            end,[23404]=function()
                Q[1]=Q[3][Q[1]]
                ba=ae(28718)
            end,[21544]=function()
                Q[29]=Q[1];
                if not(Q[29]==0)then
                    ba=ae(12772)
                    return true
                else
                    ba=ae(-31896)
                    return true
                end
                ba=ae(9476)
            end,[3618]=function()
                Q[2]+=Q[1];
                ba=-11933;
            end,[11976]=function()
                Q[2]+=Q[1];
                ba=ae(27696)
            end,[21791]=function()
                Q[62]=Q[1];
                ba=ae(15930)
            end,[5113]=function()
                Q[1]=Ab[Q[1]]
                ba=-18135
            end,[-24745]=function()
                Q[62]=Q[1];
                if Q[31]==0 then
                    ba=ae(14036)
                    return true
                else
                    ba=ae(9961)
                    return true
                end;
                ba=ae(-2179);
            end,[7004]=function()
                Q[1]=Q[9]
                ba=ae(6105)
            end,[6899]=function()
                Q[11]=Q[3][Q[11]]
                ba=889
            end,[-4942]=function()
                Q[1]=15917
                ba=ae(9296)
            end}
            ba=ae(4691)
            repeat
                while true do
                    Q[65]=Bc[ba]
                    if Q[65]~=nil then
                        if Q[65]()then
                            break
                        end
                    elseif ba==22275 then
                        if not(Q[44]==156)then
                            ba=ae(18472)
                            break
                        else
                            ba=ae(-5084)
                            break
                        end
                        ba=ae(20379)
                    elseif ba==-24662 then
                        if not(Q[3][29290]==241)then
                            ba=ae(-25906)
                            break
                        else
                            ba=ae(-14119)
                            break
                        end
                        ba=ae(16736)
                    elseif ba==-12959 then
                        if not(Q[44]==44)then
                            ba=ae(18252)
                            break
                        else
                            ba=ae(13169)
                            break
                        end
                        ba=ae(6672)
                    elseif ba==-29991 then
                        if Q[44]==57 then
                            ba=ae(-30606)
                            break
                        elseif Q[44]==192 then
                            ba=ae(-1159)
                            break
                        elseif Q[44]==116 then
                            ba=ae(2769)
                            break
                        elseif Q[44]==122 then
                            ba=ae(-21648)
                            break
                        elseif not(Q[44]==207)then
                            ba=ae(-29989)
                            break
                        else
                            ba=ae(-13949)
                            break
                        end
                        ba=ae(-12185)
                    elseif ba==22154 then
                        Q[66]=false;
                        ba=ae(-27303);
                    elseif ba==21987 then
                        Q[30],Q[27]=Q[1],Q[9];
                        ba=22674;
                    elseif ba==28543 then
                        if Q[44]==174 then
                            ba=ae(24522)
                            break
                        elseif Q[44]==80 then
                            ba=ae(19658)
                            break
                        elseif Q[44]==189 then
                            ba=ae(-30566)
                            break
                        elseif not(Q[44]==91)then
                            ba=ae(-26732)
                            break
                        else
                            ba=ae(15238)
                            break
                        end
                        ba=ae(31115)
                    elseif ba==8179 then
                        if Q[3][29290]==128 then
                            ba=ae(-9360)
                            break
                        elseif not(Q[3][29290]==232)then
                            ba=ae(-13807)
                            break
                        else
                            ba=ae(-278)
                            break
                        end
                        ba=ae(28511)
                    elseif ba==4608 then
                        Q[54],Q[2],Q[45],Q[61],Q[66]=Q[1],Q[9],Q[10],Q[11],false;
                        ba=ae(-8837);
                    elseif ba==-16236 then
                        if not(not Q[66])then
                            ba=ae(28882)
                            break
                        else
                            ba=ae(28684)
                            break
                        end
                        ba=ae(15658)
                    elseif ba==-3499 then
                        if not(Q[3][29290]==31)then
                            ba=ae(9265)
                            break
                        else
                            ba=ae(18535)
                            break
                        end
                        ba=ae(-18280)
                    elseif ba==31186 then
                        Q[2]+=Q[1];
                        if Q[44]==137 then
                            ba=ae(3260)
                            break
                        elseif Q[44]==7 then
                            ba=ae(12078)
                            break
                        elseif Q[44]==20 then
                            ba=ae(-24074)
                            break
                        elseif not(Q[44]==158)then
                            ba=ae(4285)
                            break
                        else
                            ba=ae(10237)
                            break
                        end
                        ba=ae(-16025)
                    elseif ba==-2433 then
                        return se(Q[1][1],1,Q[1][2])
                    elseif ba==-18527 then
                        Q[41]=Q[1];
                        if Q[41]==Qb then
                            ba=ae(-17571)
                            break
                        else
                            ba=ae(15343)
                            break
                        end
                        ba=ae(2786)
                    elseif ba==-8723 then
                        if not(Q[44]==188)then
                            ba=ae(25457)
                            break
                        else
                            ba=ae(17431)
                            break
                        end
                        ba=ae(5293)
                    elseif ba==-27676 then
                        if not(Q[44]==134)then
                            ba=ae(-23730)
                            break
                        else
                            ba=ae(23478)
                            break
                        end
                        ba=ae(10456)
                    end
                end
            until ba==7221
        end
        local Ec,Tb,I,uc
        uc={}
        Ec,I={[19824]=-14418,[-13329]=-17670,[-13283]=-14418},function(R)
            return Ec[R+-16353]
        end
        Tb=I(3024)
        repeat
            while true do
                if Tb==-14418 then
                    return uc[1]
                elseif Tb==-17670 then
                    uc[1]=function(...)
                        local me,ab,He,Nd,na
                        Nd={}
                        me,na={[23516]=-26503,[-2499]=2620,[-17248]=-32307,[-18723]=-10793,[22483]=5635,[-2048]=23141,[-28130]=-16325,[18606]=-28688,[-13638]=-18517,[-967]=-862,[32388]=-5701,[-30670]=-24544,[-921]=6957,[11240]=-29195,[26056]=31151,[-13669]=15696,[-3054]=20221,[13899]=-10793,[2656]=23141,[-24689]=-23314,[-4797]=-5701,[27872]=2256,[26576]=-24544,[258]=-26503,[22590]=-29476,[-17020]=-23806,[-13491]=2256,[24587]=-29476,[-22542]=13613,[-22263]=-16055,[-26500]=3341,[-19939]=23695,[-853]=-2081,[8688]=-18313,[-6589]=5771,[-14851]=14460,[7778]=-12592,[-10333]=24500,[9420]=-862,[28462]=5771,[1580]=-29476,[-30779]=28292,[-6634]=25963,[12448]=2415,[26468]=-4018,[2240]=-18517,[15699]=-7677,[-32217]=-20867,[29379]=-4018,[25621]=-12739,[20975]=5635,[-26378]=-20472,[-19982]=-10793,[-11726]=31151,[28379]=-4018,[-19104]=20023,[-30756]=-4018,[3464]=-8921,[29123]=2826,[-20228]=19747,[-5170]=14460,[-9479]=-29476,[-20206]=15696,[31731]=-23314,[16190]=2620,[31046]=9842,[-31681]=-10306,[-19912]=6780,[-8270]=30045,[23817]=6780},function(Oc)
                            return me[Oc+-8390]
                        end
                        He={[20344]=function()
                            Nd[1]=0
                            ab=na(-23291)
                        end,[30746]=function()
                            Nd[2]=Wd[Nd[2]]
                            ab=na(-8630)
                        end,[22923]=function()
                            Nd[1]=-32174
                            ab=na(-11549)
                        end,[27198]=function()
                            Nd[1]=Wd[Nd[1]]
                            ab=12529
                        end,[-24544]=function()
                            ab=na(-17988);
                            return true;
                        end,[-12739]=function()
                            Nd[2]=bb(Mb(Nd[3],Nd[2]))
                            ab=na(11046)
                        end,[30261]=function()
                            Nd[4]=1
                            ab=2458
                        end,[20221]=function()
                            Nd[2]=Wd[Nd[2]]
                            ab=-22159
                        end,[4368]=function()
                            Nd[4]=Wd[Nd[4]]
                            ab=20349
                        end,[-23806]=function()
                            Nd[4]=1
                            ab=30051
                        end,[28718]=function()
                            Nd[4]=Wd[Nd[4]]
                            ab=na(-19740)
                        end,[-18313]=function()
                            Nd[5]=13017
                            ab=4924
                        end,[-29430]=function()
                            Nd[1]=23720
                            ab=27198
                        end,[-29476]=function()
                            Nd[2]=0
                            ab=na(34011)
                        end,[-12987]=function()
                            Nd[2]=he(se(Nd[2][1],1,Nd[2][2]))
                            ab=na(-5279)
                        end,[-20867]=function()
                            Nd[2]=-32174
                            ab=-29868
                        end,[-16325]=function()
                            Nd[2]=bb(y(Ua,Nd[6],Nd[2],Nd[4],Nd[7]))
                            ab=na(-13873)
                        end,[-7677]=function()
                            Nd[1]=Nd[8][Nd[1]]
                            ab=na(20838)
                        end,[-22159]=function()
                            Nd[4]=-13904
                            ab=28718
                        end,[5771]=function()
                            Nd[4]=Nd[9].n
                            ab=-29430
                        end,[12529]=function()
                            Nd[4]=Nd[4]-Nd[1]
                            ab=na(17810)
                        end,[2620]=function()
                            Nd[7][13017]=Nd[10];
                            ab=na(-18110);
                        end,[23695]=function()
                            Nd[1]=Nd[7][Nd[1]]
                            ab=20939
                        end,[15982]=function()
                            Nd[4]=Wd[Nd[4]]
                            ab=20344
                        end,[-12592]=function()
                            Nd[4]=1
                            ab=22923
                        end,[-29195]=function()
                            Nd[2]=1
                            ab=12440
                        end,[6957]=function()
                            Nd[2]=bb(sd(Nd[3]))
                            ab=na(30873)
                        end,[-29868]=function()
                            Nd[2]=Nd[8][Nd[2]]
                            ab=31939
                        end,[-862]=function()
                            Nd[11],Nd[10]=Nd[2],Nd[4];
                            ab=na(24580);
                        end,[30051]=function()
                            Nd[2]=Nd[2]+Nd[4]
                            ab=na(1801)
                        end,[24500]=function()
                            Nd[2]=Nd[8][Nd[2]]
                            ab=na(39436)
                        end,[-16055]=function()
                            Nd[2]=bb(Wb(se(Nd[2][1],1,Nd[2][2])))
                            ab=na(-6461)
                        end,[20939]=function()
                            Nd[2]=ec(Nd[9],Nd[11],Nd[2],Nd[4],Nd[1])
                            ab=na(34858)
                        end,[5635]=function()
                            Nd[3]=se(Nd[2][1],1,Nd[2][2]);
                            ab=na(30980)
                        end,[12440]=function()
                            Nd[4]=23720
                            ab=15982
                        end,[-1734]=function()
                            Nd[2]=Nd[2][Nd[4]]
                            ab=na(-3336)
                        end,[2458]=function()
                            Nd[2]=Nd[2]-Nd[4]
                            ab=na(16168)
                        end,[9842]=function()
                            Nd[4]=2
                            ab=-1734
                        end,[20023]=function()
                            Nd[2]=23720
                            ab=30746
                        end,[2415]=function()
                            Nd[2]=bb(Pb(Nd[2],Nd[4],Nd[1]))
                            ab=na(36262)
                        end,[-32764]=function()
                            Nd[1]=13017
                            ab=na(24089)
                        end,[3341]=function()
                            Nd[2]=Nd[11]+Nd[10]
                            ab=30261
                        end,[-10306]=function()
                            Nd[2]=ec(Nd[9],Nd[2],Nd[4],Nd[1],Nd[6])
                            ab=na(40121)
                        end,[-26503]=function()
                            Nd[2]=-32174
                            ab=na(-1943)
                        end,[-29989]=function()
                            Nd[1]={[Nd[1]]=Nd[12],[Nd[5]]=Nd[13]}
                            ab=na(40778)
                        end,[6780]=function()
                            Nd[1]=-32174
                            ab=-28158
                        end,[-28158]=function()
                            Nd[12]={}
                            ab=na(17078)
                        end,[20349]=function()
                            Nd[4]=Qc(Nd[4])
                            ab=na(32207)
                        end,[-4018]=function()
                            Nd[2]=4460
                            ab=na(5336)
                        end,[31939]=function()
                            Nd[4]=2
                            ab=-32764
                        end,[4924]=function()
                            Nd[13]=0
                            ab=-29989
                        end,[15696]=function()
                            Nd[4]=566
                            ab=4368
                        end}
                        ab=na(10630)
                        repeat
                            while true do
                                Nd[14]=He[ab]
                                if Nd[14]~=nil then
                                    if Nd[14]()then
                                        break
                                    end
                                elseif ab==14460 then
                                    Nd[8]=se(Nd[2][1],1,Nd[2][2]);
                                    if not(Nd[8][-32174][1])then
                                        ab=na(8648)
                                        break
                                    else
                                        ab=na(-23827)
                                        break
                                    end
                                    ab=-24544
                                elseif ab==-5701 then
                                    Nd[9],Nd[6],Nd[7]=Nd[2],Nd[4],Nd[1];
                                    ab=na(19630);
                                elseif ab==31151 then
                                    Nd[3]=Nd[2];
                                    if not(fd(Nd[3])==false)then
                                        ab=na(9970)
                                        break
                                    else
                                        ab=na(7469)
                                        break
                                    end
                                    ab=na(32977)
                                elseif ab==2256 then
                                    return se(Nd[2][1],1,Nd[2][2])
                                elseif ab==23141 then
                                    return se(Nd[2][1],1,Nd[2][2])
                                elseif ab==-18517 then
                                    Nd[2]=bb(...)
                                    ab=-12987
                                elseif ab==-23314 then
                                    if Wd[23720]<Nd[9].n then
                                        ab=na(-10714)
                                        break
                                    end
                                    ab=na(37769)
                                end
                            end
                        until ab==-20472
                    end
                    Tb=I(36177)
                end
            end
        until Tb==-2125
    end
    local ye,G,q,p,Da
    q={}
    Da,G={[-1681]=18957,[9487]=-6431,[-23372]=-14077,[-16354]=-6431,[23773]=27822,[-27499]=18957},function(La)
        return Da[La-9950]
    end
    ye={[18957]=function()
        q[1]=bb(ze(M,od))
        p=G(-6404)
    end}
    p=G(8269)
    repeat
        while true do
            q[2]=ye[p]
            if q[2]~=nil then
                if q[2]()then
                    break
                end
            elseif p==-6431 then
                return se(q[1][1],1,q[1][2])
            end
        end
    until p==8254
end
local yd,vd
local Md,gc,Z,le,nd,vc
Z={}
le,gc={[-28528]=-25936,[-2557]=-28872,[31865]=-29623,[-8477]=-7146,[-9050]=26839,[-29370]=-25936,[20306]=-7146,[-8455]=18491,[-16389]=-18244,[6387]=-25936},function(Ie)
    return le[Ie- -11655]
end
Md={[-25936]=function()
    yd,vd=Z[1],function()
        local re,Fe,Ac,Zb,xe
        Fe={}
        Ac,xe={[21066]=-1770,[1781]=10481,[-10487]=16006,[-18940]=-4174,[-1918]=10481,[1228]=16006,[-28861]=-14475,[-13449]=17505,[-22230]=-4174},function(v)
            return Ac[v+-9561]
        end
        Zb={[16935]=function()
            Fe[1]=3
            re=-2682
        end,[17505]=function()
            Fe[2]=yd[Fe[2]]
            re=-6001
        end,[-6001]=function()
            Fe[3]={[Fe[3]]=yd,[Fe[1]]=Fe[2]}
            re=xe(-12669)
        end,[28562]=function()
            Fe[3]=2
            re=16935
        end,[-2682]=function()
            Fe[2]=0
            re=xe(-3888)
        end,[-1770]=function()
            Fe[3]=yd[Fe[3]]
            re=29370
        end,[29370]=function()
            Fe[1]=1
            re=-21189
        end,[-21189]=function()
            Fe[3]=Fe[3]+Fe[1]
            re=xe(10789)
        end,[10481]=function()
            Fe[3]=0
            re=xe(30627)
        end}
        re=xe(11342)
        repeat
            while true do
                Fe[4]=Zb[re]
                if Fe[4]~=nil then
                    if Fe[4]()then
                        break
                    end
                elseif re==16006 then
                    yd[0]=Fe[3];
                    re=28562;
                elseif re==-4174 then
                    return Fe[3]
                end
            end
        until re==29086
    end;
    nd=gc(8651);
end,[-29725]=function()
    Z[2]=0
    nd=gc(-20110)
end,[18491]=function()
    Z[1]={[Z[1]]=Z[2]}
    nd=gc(-41025)
end,[-7146]=function()
    vc=ge;
    nd=gc(-14212);
    return true;
end,[-29623]=function()
    Z[1]=0
    nd=-29725
end}
nd=gc(20210)
repeat
    while true do
        Z[3]=Md[nd]
        if Z[3]~=nil then
            if Z[3]()then
                break
            end
        end
    end
until nd==-28872
local Za,Ce,wa,pe,nc
nc={}
wa,Ce={[-5431]=14523,[16167]=24117,[20614]=-11427,[24269]=6412,[-12075]=17324,[17618]=6412,[10883]=-11427},function(Dd)
    return wa[Dd+-21234]
end
Za={[-11427]=function()
    nc[1]=bb((function()
        local Rb,Be,E,pa,r
        E={}
        r,Rb={[9652]=-26113,[30471]=-12967,[-2133]=30717,[1605]=1314,[28217]=1314,[32287]=-12967,[-17015]=3291},function(ve)
            return r[ve+-30799]
        end
        pa={[-12967]=function()
            E[1]={}
            Be=Rb(13784)
        end,[3291]=function()
            E[1]=bb(vc(rb(Ad('/7mysjIuWX+0/wFSJ+l8Uher91BSJgBwJ6nhdv8n6fVQJenhd38n6fFEJ+l/AfCvpe3VhQJ2UwJ0dLYCcFEjAnQm6AJz1P+F4okn6+F1Jz/pFmMn63kBMANy//ana1BRL+l8/05HaaXu1odF/1Am6bxS0RbhfXgEMFcu6eF5BDDvUSbqdgQy1oVF+1E/A3Pm52tQVv8q6Xxil2nhfPoCsSkB8NGE4s4j9eoDslYDsH0n6eBvUSNTcwLw4WIAsfcmzWwHdVI3qlC7VjUAcajhYAPxNP/ppe7RhMBSIEoB/2EB8TMB8gG+ZgGw31Qy6eFnAHBVMZ/pBlch7gJ/AnBkqgQxMAQ4VwGwPgBwp/+oaFc96aXs0aeEs1YNtANxVAN0aqoFcTwDcNMDd1QDc3v/IVzUCFIk6KW/7NOERVQFA7S376pQWjoAcOeo4f1oA7BbOeniziD/46Xu3YXAUj4qAjdUAjRpBfE4BfgBt+/iziLhATPnqlD7VQYdsaul7dKE1AK3CnhvBHEFBHL6Uv/W6uJ/JukcL/YkQLMmAFGccZZ197CzJQChA5tCKt+SUSCzKADBlEH/d+1OoVJq3U77szMA8StVal5D/6tNzBxUSUZFv7pN3xyzKQFhlv/5uJ2Givq6hXuPqATzE6sUoQCTf/VO61uwsy8Aof8bV9tk0nkxUH/PcP1dG7MsASH/nYJ/CXQVaqj/n1IFWxB8syr+ATE1VRbjHVUG9/KzIQDRVF9NsP4GQwu6+QPyPJrv4gnoLQWj4aPC96ezJACRvE7mie9B4LMnALHWxs/f2tKwsw4AscAm/97vAVDUZcML/x+H+k/HXovU/7IX1/LfBcTp/wlcyFCCKjmn/9EcgniP2L0H75j7sxIDEXnfgf/2ZQEgJvx1vf/Zl2po11ZZD/910ZgFPFf0gv/+bwwzCJ9qnP+T2Htj8U9SFX9zkNsufbM1A1H/nrivLM/eeoL/zb27idSkKM3/0ma2iYnT5bP9QQHB7uw6ts1o/0/1N7bhfml5/3PZHJrsD9WY/1fX4dm327ck/1HsFCsL+6P1/543uUX3TeI0/+HDlOf7JbPO/30SvyOkuThr/3FjwkaV+BXe/8VH3eLypty3/39e7CZrAfz5//iAZ65G9Uq/f3Grmo7ysz4Ggf/iz6Oqx7k85/8TQoRExjTe4v+ir8z8IZp7dO+HGJp/EbNqqntfpXawsSIfggEAUf8A6QyAmryIef/adUC8pKrsXf/YLn/xcBokZP9Llx1Gq+eq5/dL1HoWw1lYXFf3sLMIAKFXYA24/8sSt964Zt1W/wIb+NjJzOE1/5N+ovVC81yG/8PqeZxFDBvs/9DT0uUryLM//gLxYhs2BW9si/9ldWRUJXwnWv+Y2ftXK69lJ79kVAxmZQMfM/v/krHS/ZSswbD7s3EA4Y7HKvgR/+He9EpsIgXa/yUuCR/BrwOv/4KmTumDiRXH/zKif6UwlETz/9S5UyuHaZes/+Xh+V6aaXFC/8NgEBUYiKi1/2vOvE+mwIkb/80vu3VG5ZZe/6DTuRcR+7MgugZyECVTU/cjERPT7yQDvisIM3kwGt8yKsgsuAGD2pa32/zqA0NnygBzgv0vJtNfOK4DvhD+MtPb6xFtMnwYfhIzfDtll7MjAJF9OgRDWd+UbIYDw7uvwgeT7C+UAQOm97SzNABxm2qxjv9lNaY441l66v+VO6Ib/bHcY+/31bM6AbEik+7/lwFkUnvcJ9j/tPgeDZIgPGHfAfG/QVMxA8X0/8BNq7sI3+3o9+mzEwEBhphOxv++UmDaRdeWUP8OEAexafiZfP+w6jS4j63Stf/DCjKDeQRjmv/XlPUGi3TFYt+NWIXu9jhDuYP/zMuj3E66MwFv5Ta0UQnTGsk/A/9bC5AUdQQHJP/6PTXehM+UOL27AeMuObM3AHEM/4KQNJYy8qzK/wn9N207nLG677yAR/IFUzn5q/+JMkzOFwcLdPdEEwcSA4/9wqb7swsAkdUlt29M/8GanSkNGCWs/0BcYHmo3nbK/6LVK8nsqXao/1zWGmaK8CvT77DYc34G4raysswI0k1TR6lvFVcUqVB7UyQAcAep4VptlN1baVAqJuhsUaXv+9SFcFCGpe/Vh8/ifyfpTXJN08xl//9SU9R491hHfbAJ8z1wN3SwO1P/98WHwdSGB6z/SSCb7+LISJf/IpG7jVQXoSr/BDf3bUpvh4D/r6Z+hhhl44j/ODKKjwuQM/T/n/7SmMTXk1r/5l0yw6ngwFj/jHier5dfSrH/IAcc5mpKNIj/gJ3mdIFCaP3/2C8xiIhW1Xn3rYXrJBO+pMWG95fUgiMDoAqeNu0gFiNaER1Dn7Hz/7GnvCiDO0Hs/8GFoTz/V/PO//4xZcJFakOS/wM+QeTvwokW/+dsBIi5CgO4v6Bk/FfZ/SZz9u3QPmO2shyD+lLPV+nhVBHgUHbAVRJh+ybpEuHxOSfpfk4SYe7VgmYEFeAjAHBvF6nhWAJwUyIBsdmEYwEnUrMuAFFmp//d7ve4Wbzd/vvnuQETW/VvCU1/EkHlaitIDxXj/2UyJHcXRikg+3c2DcMLPhIiD/ewsxcAsQg4lDH/b5DLerwARZb/jyTixf+xrJb/3wAdECSMJ6//e4KgTSpR9ST/V5zBIOyXuf3/rNfLBAYtNYH9fTfjCsi8ipLKfgCzM4ANSixgBePfI1tWEkQTs3tM+7M4PmLaCKWAKP9EJ5xFM/PhUP+N5djdw/e7ZQ9vQ1DtEbI=')),E[1]))
            Be=Rb(32404)
        end}
        Be=Rb(61270)
        repeat
            while true do
                E[2]=pa[Be]
                if E[2]~=nil then
                    if E[2]()then
                        break
                    end
                elseif Be==1314 then
                    return se(E[1][1],1,E[1][2])
                end
            end
        until Be==15811
    end)())
    pe=1126
end,[17324]=function()
    nc[1]=bb(se(nc[1][1],1,nc[1][2])(se(nc[2][1],1,nc[2][2])))
    pe=Ce(38852)
end}
pe=Ce(32117)
repeat
    while true do
        nc[3]=Za[pe]
        if nc[3]~=nil then
            if nc[3]()then
                break
            end
        elseif pe==6412 then
            return se(nc[1][1],1,nc[1][2])
        elseif pe==1126 then
            nc[2]=bb(...)
            pe=Ce(9159)
        end
    end
until pe==31870 end)(...)