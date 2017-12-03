#ライブラリの読み込み
import urllib
from bs4 import BeautifulSoup
from urllib.request import urlopen
import requests
from urllib.error import HTTPError
import csv, time

#URLのリストを読み込む
f = open('url_list.csv', 'r')
dataReader = csv.reader(f)

#結果の出力用のリストを作る
data01 =[] #URL
data02 =[] #順位
data03 =[] #title&artist
data04 =[] #link
data05 =[] #artist

for row in dataReader:
       for url in row:
            time.sleep(10.0) #sleep(秒指定)
            try:
                    r = requests.get(url)
                    soup =  BeautifulSoup(r.content, 'html.parser')
                    
                    for body in soup.findAll("tbody"):
                        for detail in body.findAll("tr"):
                            for ranking in detail.findAll("td",{'class':'rank_td'}):
                                for content in detail.findAll("div",{'class':'name_detail'}):
                                    for artist_name in content.findAll("span"):
                                        for link in artist_name.findAll("a"):
                                            data01.append(url)
                                            data02.append(''.join(ranking.findAll(text=True)))
                                            data03.append(''.join(content.findAll(text=True)))
                                            data04.append(link.get("href"))
                                            data05.append(''.join(artist_name.findAll(text=True)))
                                            data = zip(data01,data02,data03,data04,data05)
                                            #CSV出力
                                            with open('result.csv','wt',errors='backslashreplace') as fout:
                                                writecsv = csv.writer(fout,lineterminator='\n')
                                                writecsv.writerows(data)                                   
                                
                                    
            except HTTPError as e:
                print(e.code)