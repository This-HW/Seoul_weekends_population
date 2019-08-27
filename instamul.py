from bs4 import BeautifulSoup
import selenium.webdriver as webdriver
import urllib.parse
from urllib.request import Request, urlopen
from time import sleep
import pandas as pd
from multiprocessing import Pool, Value, freeze_support

num = 0 

def f(x):   
    frame = []
    global num 
    
    req = Request('https://www.instagram.com/p'+x,headers={'User-Agent': 'Mozilla/5.0'})
    webpage = urlopen(req).read()
    soup = BeautifulSoup(webpage,"lxml",from_encoding='utf-8')
    soup1 = soup.find("meta",attrs={"property":"og:description"})
    reallink1 = soup1['content']
    reallink1 = reallink1[reallink1.find("@")+1:reallink1.find(")")]
    reallink1 = reallink1[:20]
    if reallink1 == '':
        return
    #mylist.append(reallink1)
        
    for reallink2 in soup.find_all("meta",attrs={"property":"instapp:hashtags"}):
    
        reallink2 = reallink2['content']
        reallink2 = reallink2[:10]
        mylist = []
        
        mylist.append(reallink1)
        mylist.append(reallink2)
        
        frame.append(mylist)

    print(str(num)+"개의 데이터 저장 중")
    num += 1
    data = pd.DataFrame(frame)
    data.to_csv('insta.txt', mode='w',encoding='utf-8',header=None)

    


if __name__ == '__main__':
    freeze_support()

    print('### jinho021712@gmail.com ### Instacrawler Ver 0.1')
    
   
    print("pass")
    print("#크롤링 속도는 컴퓨터 사양에 따라 1.0 ~ 2.5 값으로 설정해주세요.")

    scrolltime = float(input("크롤링 속도를 입력하세요 : "))
    crawlnum = int(input("가져올 데이터의 수를 입력하세요 : " ))
    search = input("검색어를 입력하세요 : " )
    search = urllib.parse.quote(search)
    url = 'https://www.instagram.com/explore/tags/'+str(search)+'/'
    driver = webdriver.Chrome('chromedriver.exe')

    driver.get(url) 
    sleep(5)


    SCROLL_PAUSE_TIME = scrolltime
    reallink = []

    while True:
        pageString = driver.page_source
        bsObj = BeautifulSoup(pageString, "lxml")

        for link1 in bsObj.find_all(name="div",attrs={"class":"Nnq7C weEfm"}):

                title = link1.select('a')[0] 
                real = title.attrs['href']
                reallink.append(real) 
                title = link1.select('a')[1] 
                real = title.attrs['href']
                reallink.append(real) 
                title = link1.select('a')[2] 
                real = title.attrs['href']
                reallink.append(real)



        last_height = driver.execute_script("return document.body.scrollHeight")
        driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
        sleep(SCROLL_PAUSE_TIME)
        new_height = driver.execute_script("return document.body.scrollHeight")
        if new_height == last_height:
            driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            sleep(SCROLL_PAUSE_TIME)
            new_height = driver.execute_script("return document.body.scrollHeight")

            if new_height == last_height:
                break

            else:
                last_height = new_height
                continue

    reallinknum = len(reallink)
    print("총"+str(reallinknum)+"개의 데이터를 받아왔습니다.")

    p = Pool(5)
    p.map(f, reallink)
    p.close()
    p.join()
    print("저장완료")
 
