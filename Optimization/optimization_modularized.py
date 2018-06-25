# -*- coding: utf-8 -*-
"""
Created on Wed Jun 20 18:16:04 2018

@author: akshay.gupta
"""


"""
Created on Mon Feb  5 16:02:38 2018

@author: akshay.gupta
"""
import pandas as pd
import sklearn.neighbors
import numpy as np
import sklearn.preprocessing
import pickle as pk
import datetime
from scipy.optimize import minimize
from scipy.optimize import basinhopping
from scipy.optimize import differential_evolution


df = pd.read_csv("concatenated_subset_for_optimization.csv")
y_def = pd.read_csv("Data/Y_def.csv")
df = df.merge(y_def, on = ["fortnight","ID"])

var_list = ['var1',	'var2',	'var3']

op_var_list = ['OUTPUT_final_' + x for x in var_list]

#df_aks = df.copy()
norm = list( set(op_var_list))
level = ["ID", "fortnight","city", "category", "city_tier", "peer", "city_tier_peer", "cnt", "disbursal_flag", "NPA","Y_DEF"]
df.set_index(level, inplace = True)

df[df >=0.9] = 0
df[df <=-0.9] = 1
df.reset_index(inplace = True)

df_val = df[(df["fortnight"]<=28) | (df["fortnight"]>=46)]
df_tmp = df[(~(df["fortnight"]<=28)) & (~(df["fortnight"]>=46))]
df_tr = df_tmp.sample(frac=0.70, random_state=99)
df_tr_tmp = df_tr.loc[:,["ID", "fortnight"]].copy()
df_tr_tmp["tmp_value"] = 1
df_test_tmp = df_tmp.merge(df_tr_tmp, on = ["ID", "fortnight"], how = "left")
df_test = df_test_tmp[np.isnan(df_test_tmp["tmp_value"])]

del df_test_tmp
del df_tr_tmp
del df


now = datetime.datetime.now()
start_time = now.strftime("%Y-%m-%d %H:%M")

non_nomal = ["ID", "fortnight","city", "category", "city_tier", "peer", "city_tier_peer", "cnt", "disbursal_flag", "NPA","Y_DEF"]

def aks(yo, op_panel, cut_off, non_nomal):
    wt_dic = pd.DataFrame()
    for cat in ['Non Digital-OM', 'LFR', 'Modern Retail', 'Digital - OM']:
		
        def summary_trigger(tag_var, data, var, trig_flag):
            trig = sum(data.loc[data[tag_var] == trig_flag, var])
            return (trig)
			
				
        def summary(data, tag_var, metrics):
            ret = []
            if metrics["trig"] == 1:
                trig = summary_trigger(tag_var, data = data, var= "tag_"+yo+"_"+"nm_"+str(cut_off), trig_flag = 1)
                ret.append(trig)
            
            if metrics["tot"] == 1:
                tot = len(data)
                ret.append(tot)
                
            if metrics["disb_trig"] != "":                
                disb_trig = summary_trigger(tag_var, data = data, var = metrics["disb_trig"], trig_flag = 1)
                ret.append(disb_trig)
            
            if metrics["disb_non_trig"] != "":
                disb_non_trig = summary_trigger(tag_var, data = data, var = metrics["disb_non_trig"], trig_flag = 0)
                ret.append(disb_non_trig)
			
            if metrics["npa_trig"] != "":
                npa_trig = summary_trigger(tag_var, data = data, var = metrics["npa_trig"], trig_flag = 1)
                ret.append(npa_trig)
            
            if metrics["npa_non_trig"] != "":
                npa_non_trig = summary_trigger(tag_var, data = data, var = metrics["npa_non_trig"], trig_flag = 0)
                ret.append(npa_non_trig)
			
            if metrics["cnt_trig"] != "":
                cnt_trig = summary_trigger(tag_var, data = data, var = metrics["cnt_trig"], trig_flag = 1)
                ret.append(cnt_trig)
            
            if metrics["cnt_non_trig"] != "":
                cnt_non_trig = summary_trigger(tag_var, data = data, var = metrics["cnt_non_trig"], trig_flag = 0)
                ret.append(cnt_non_trig)
				
            if metrics["Y_Cap"] != "":					
                Y_Cap = sum(data.loc[(data["tag_"+yo+"_"+"nm_"+str(cut_off)]==1) & (data[metrics["Y_Cap"]]==1), metrics["Y_Cap"]])
                ret.append(Y_Cap)
            
            if metrics["Y_total"] != "":
                Y_total = sum(data.loc[:, metrics["Y_total"]])
                ret.append(Y_total)
			
            return(ret)
		
        def op(x):
            df1 = df_t.copy()
            for i in range(0, len(x)):
                df1["Score"] = df1["Score"] + df1[norm1[tmp+i]]*x[i]			
            df1["rank"] = df1['Score'].rank(method = "first", ascending=False)
            df1["tag"] = (df1["rank"]<=len(df1)*cut_off).astype(int)
            aks = df1.loc[(df1["tag"]==1)&(df1["Y_DEF"]==1),["Y_DEF"]] 
            true_po = sum(aks.iloc[:,0])
            return (-true_po)
		
		
        tmp  = len(non_nomal)
		
        non_nomal.append(op_panel)
        norm1 = non_nomal
		
        df_t = df_tr.loc[df_tr["category"] == cat,norm1].copy()
        df_t_test = df_test.loc[df_test["category"] == cat,norm1].copy()
        df_t_val = df_val.loc[df_val["category"] == cat,norm1].copy() 
		
        x0 = np.random.rand(164)
        res = minimize(op, x0, method='nelder-mead',options={'xtol': 1e-30, 'disp': True, 'maxiter':1000})
        wt = res.x
		
        for i in range(0, len(x0)):
            df_t["Score_"+yo+"_"+"nm_"+str(cut_off)] = df_t["Score_"+yo+"_"+"nm_"+str(cut_off)]+df_t[norm1[tmp+i]]*wt[i]
		
        df_t["rank_"+yo+"_"+"nm_"+str(cut_off)] = df_t["Score_"+yo+"_"+"nm_"+str(cut_off)].rank(method = "first", ascending=False)
        df_t["tag_"+yo+"_"+"nm_"+str(cut_off)] = (df_t["rank_"+yo+"_"+"nm_"+str(cut_off)]<=len(df_t)*cut_off).astype(int)        
		
		
        
        metrics = {"trig" : 1, "tot" : 1, "disb_trig" : "disbursal_flag", "disb_non_trig" : "disbursal_flag"
                   , "npa_trig" : "NPA", "npa_non_trig" : "NPA", "cnt_trig" : "cnt", "cnt_non_trig" : "cnt","Y_Cap" : "Y_DEF","Y_total" : "Y_DEF"}
        
        tag_var = "tag_"+yo+"_"+"nm_"+str(cut_off)
        
        summary_tr = summary(df_t, tag_var, metrics)
        
        score_t = min(df_t[(df_t["tag_"+yo+"_"+"nm_"+str(cut_off)]==1)]["Score_"+yo+"_"+"nm_"+str(cut_off)])
    		
        for i in range(0, len(x0)):
            df_t_test["Score_"+yo+"_"+"nm_"+str(cut_off)] = df_t_test["Score_"+yo+"_"+"nm_"+str(cut_off)]+df_t_test[norm1[tmp+i]]*wt[i]
		
        
        df_t_test["tag_"+yo+"_"+"nm_"+str(cut_off)] = (df_t_test["Score_"+yo+"_"+"nm_"+str(cut_off)]>=score_t).astype(int)        
        
        summary_test = summary(df_t_test, tag_var, metrics)
        
        for i in range(0, len(x0)):
            df_t_val["Score_"+yo+"_"+"nm_"+str(cut_off)] = df_t_val["Score_"+yo+"_"+"nm_"+str(cut_off)]+df_t_val[norm1[tmp+i]]*wt[i]
		
        
        df_t_val["tag_"+yo+"_"+"nm_"+str(cut_off)] = (df_t_val["Score_"+yo+"_"+"nm_"+str(cut_off)]>=score_t).astype(int)        
        
        summary_val = summary(df_t_val, tag_var, metrics)
        
        lev = ["nelder-mead", cat, yo, cut_off, wt]
        lev.append(summary_tr.append(summary_test.append(summary_val)))
        
        wt_dic = wt_dic.append([[lev]])        		
        return wt_dic


wt_dic = aks("PANEL_", op_var_list, 0.1)
now = datetime.datetime.now()
end_time = now.strftime("%Y-%m-%d %H:%M")
print (start_time,end_time)
wt_dic.columns = ["optimzer","category","prd", "cut_off","weights", "trig","total","disb_trig", "disb_non_trig", "npa_trig", "npa_non_trig", "cnt_trig", "cnt_non_trig","Y_Captured","Y_total", "trig_test","total_test","disb_trig_test", "disb_non_trig_test", "npa_trig_test", "npa_non_trig_test", "cnt_trig_test", "cnt_non_trig_test","Y_Captured_test","Y_total_test", "trig_val","total_val","disb_trig_val", "disb_non_trig_val", "npa_trig_val", "npa_non_trig_val", "cnt_trig_val", "cnt_non_trig_val","Y_Captured_val","Y_total_val"]
wt_dic.to_csv("Summary_optimization_"+"panel"+".csv")