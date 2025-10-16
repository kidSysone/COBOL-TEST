# 目前的課題：
* STATE需要判斷全名?目前僅判斷簡寫
* sa文件?
* 國家名 → 是否要追加 ISO 2位數的國家CODE（例：JP, US, TW）
* 新增多種測試地址
* 原文 + 英文

***
# Cobol-Test
## 執行方式及注意事項
* 主程式: 收納於 `src\main\`
  * `EXECUTE.cob`
* 會被呼叫的程式: 收納於 `src\modules\`
  * `FORMATTER-ADDRESS.cob`
  * `OUTPUT-ADDRESS.cob`
  * `READ-RULE.cob`
* `.exe`檔: 收納於 `bin\`
```
PS C:\vs-workspaces\Cobol-Test> 
cobc -x -o bin\EXECUTE.exe src\main\EXECUTE.cob src\modules\READ-RULE.cob src\modules\FORMATTER-ADDRESS.cob src\modules\OUTPUT-ADDRESS.cob

.\bin\EXECUTE.exe
```

* 產出檔案為`.CSV`檔及`.txt`檔各2份(總資料分析結果/ 分析失敗地址清單)
  * 使用EXCEL開啟`.CSV`檔時之注意事項:
    1. 開啟空白活頁簿
    2. 資料 -> 從文字/CSV -> 匯入 -> 分隔符號:分號 -> 轉換資料
      * 標頭升階 -> 要有(使用第一個資料列作為標頭)
      * 已變更類型 -> 要拿掉
    3. 關閉並載入


***
## 功能解說
* `EXECUTE.cob`
  * 呼叫`READ-RULE.cob`，取得分類標準、國家名、城市名關鍵字清單
      * `CategoryRules.csv`: 分類標準
        * 預設上限: 40列(共210字) * 18行
        * 已做全大寫處理
        * 實際資料: 最多35列(路) * 18行，單欄最多12字，單行最長196字
      * `CountryList.csv`: 國家清單(含簡寫)
        * 預設上限: 1列(50字) * 500行
        * 已做全大寫處理、**依照文字數多->少順序排列**
        * 實際資料: 共320列，單欄最多32字
      * `WorldCitiesList.csv`: 城市清單
        * 預設上限: 1列(50字) * 50000行
        * 已做全大寫處理、**依照文字數多->少順序排列**
        * 實際資料: 最多字欄位為49字，44017列

  * 讀取`IN-FILE.csv`
    * 取得需解讀之地址清單
      * 預設上限: 1列(共500字)(IN-FILE-REC) * 行數無限制(讀完/讀取內容為空白)

  * 呼叫`FORMATTER-ADDRESS.cob`
    * `INITIALIZATION SECTION.`: 進行地址名稱的基本格式化
      * 字串中出現之","統一顯示", "(固定後接空格*1),若字串開頭","/結尾","/",,"，移除多餘","
      * 字串中出現"  "(空格重疊)，整修為" "(單一空格)
      * 字串中出現應為縮寫項目，統一後接"."("St " -> "St. ")
        * 2字: "St"、"Rd"、"Dr"、"Rm"
        * 3字: "Ave"、"Riv"
        * 4字: "Blvd"
    * `MAIN SECTION.`: 主要程序
      * 特殊字串抽出
        * 利用`CountryList.csv`尋找國家  **※ 以國家欄位一定會在字串字尾為前提**
        * 利用`WorldCitiesList.csv`尋找特殊字串
          * 若找到的字串後接`CategoryRule.csv`關鍵字，轉為依照`CategoryRule.csv`分類
      * 依照[分類規則](#分類規則)為準則分類
        * SREET 分析時，需尋找是否包含方向關鍵字
          * 全名: "NORTH"、"SOUTH"、"EAST"、"WEST"
          * 斜方: "NE"、"NW"、"SE"、"SW"
          * 簡寫: "N"、"S"、"E"、"W"、"N."、"S."、"E."、"W."
          * 羅馬字(日本): "KITA"、"MINAMI"、"HIGASHI"、"NISHI"
      * 反結構(REBUILD):
        1. FLOOR、若**字首**為純數字 -> `{***}  Floor`
        2. ROOM
        3. BUILDING
        4. NUMBER
        5. ALLEY、若**字串**為純數字 -> `Aly. {***}`
        6. LANE、若**字串**為純數字 -> `Ln. {***}`
        7. SEC、若**字串**為純數字 -> `Sec. {***}`
        8. SREET
        9. OTHER
        10. VILLAGE
        11. DISTRICT
        12. CITY
        13. PROVINCE
        14. STATE
        15. ZIP
        16. COUNTRY
    * `ERROR-SECTION.`: 進行錯誤分類(僅顯示優先順序高之項目)，下列優先度: 1 > 6
      1. 包含特殊字體: **CONTAINS INVALID CHARACTERS.**
      2. 輸入文字過長: **ADDRESS DATA IS TOO LONG.**
      3. OTHER 有值: **PARSING FAILED. PLEASE CHECK INPUT.**
      4. ZIP 為空值: **PLEASE ENTER POSTAL CODE.**
      5. COUNTRY 為空值: **PLEASE ENTER COUNTRY.**
      6. CITY 為空值: **PLEASE ENTER CITY.**  
      * ※ 若ZIP/COUNTRY/CITY有複數個欄位空值，錯誤訊息將直接串聯顯示

  * 呼叫`OUTPUT-ADDRESS.cob`
    * 整理輸出結果內容

  * 輸出所有結果:`Address_Split.txt`
    * 寫出: **TOTAL ITEMS: XXXX, ERROR ITEMS: YYYY**
  * 輸出錯誤結果清單:`Fail_Data.txt`
    * 若有錯誤資料，寫出: **ERROR ITEMS: YYYY**
    * 若無錯誤資料，寫出: **NO ERROR DATA**
  * 輸出所有結果:`Address_Split.csv`
    * 寫出: **TRESULT SUMMARY->OTAL ITEMS: XXXX, ERROR ITEMS: YYYY**
  * 輸出錯誤結果清單:`Fail_Data.csv`
    * 若有錯誤資料，寫出: **RESULT SUMMARY->ERROR ITEMS: YYYY**
    * 若無錯誤資料，寫出: **RESULT SUMMARY->NO ERROR DATA**


***
## 分類規則
* FORMATTER-ADDRESS
  * 分區判斷處理，分析內容納入`ADTER-DATA`
  1. "," -> ", "(後續需要)
    * ",," -> ","
    * 分析結果若導致地址字首/字尾為","時將該字移除
  2. 移除多餘空格
  3. 簡寫地名補上.
  4. 使用`CountryList.csv`搜尋國家名稱
  5. 使用`WorldCitiesList.csv`、`CategoryRule.csv`搜尋城市等名稱
  6. 使用空格分割字串並進行主要分段判斷(參考下表)並插入","
  7. 微調分析結果
    * ",," -> ","
    * 分析結果若導致地址字首/字尾為","時將該字移除
  8. 致力分析`ADTER-DATA`內容，不應有值


| DTLS-LF<br>之位置 | 英文名| 中文名 |FORMATTER-ADDRESS|備註|
|---|---|---|---|---|
|1|ZIP|郵遞區號|1. 荷蘭篩選<br> 2. 英式篩選<br> 3. 標準書寫/手寫常見<br>4. 為純數字或"-"所組成的字串(此條件優先度NUMBER > ZIP)|[詳細劃分規則](#詳細劃分規則)|
|2|COUNTRY|國家|依照`CountryList.csv`內容尋找國家名稱||   
|3|CITY|縣市|依照`WorldCitiesList.csv`內容尋找城市名稱<br>※ 若城市名後面連接"CITY"，視為同段字串<br>※ 若城市名後面連接方向(N/S/E/W等)，視為同段字串||
|4|DISTRICT|市區|依照`CategoryRules.csv`分類<br>≪OTHER CHECK≫<br>1. 若DISRICT為空值，則優先移動OTHER內容至CITY，並將舊CITY資料移動至DISTRICT||
|5|STREET|路|1. 依照`CategoryRules.csv`分類<br>2. "{非關鍵字字串} {方向關鍵字} {無/ROAD}"<br>3. 若STREET為空值，則優先移動OTHER內容至STREET||
|6|SEC|段|依照`CategoryRules.csv`分類|`欄位中省略關鍵字`|
|7|LANE|巷|依照`CategoryRules.csv`分類|`欄位中省略關鍵字`|
|8|ALLEY|弄|依照`CategoryRules.csv`分類|`欄位中省略關鍵字`|
|9<br>~~10~~|M-NO<br>~~S-NO~~|號|依照`CategoryRules.csv`分類<br>1. 為純數字或"-"所組成的字串(此條件優先度NUMBER > ZIP)<br>2.{數字字串} * 1以上 + {大寫英文字} * 1|原為考量x號之y分成2段，目前改不考慮的緣故10號不使用<br>`欄位中省略關鍵字`|
|11<br>~~12~~|M-FLOOR<br>~~S-FLOOR~~|樓|1. 依照`CategoryRules.csv`分類<br>2. `{數字} F.`/ `{數字} FL.`/ `{數字}F`/ `B{數字}` 之字串<br>3. 數字字串 + 序數詞(st/nd/rd/th)|原為考量x樓之y分成2段，目前改不考慮的緣故12號不使用<br>`欄位中省略關鍵字`|
|13|ROOM|室|依照`CategoryRules.csv`分類||
|14|BUILDING|建築大樓|1. 依照`CategoryRules.csv`分類<br>2. 若前一欄位塞值入FLOOR||
|15|VILLAGE|社區|依照`CategoryRules.csv`分類<br>2. 若前一欄位塞值入STREET||
|16|PROVINCE|省份|依照`CategoryRules.csv`分類||
|17|STATE|州|1.字數2~3<br>2.皆為大寫||
|18|OTHER-COL|其他|-|若字串未成功塞入任何欄位，則強制塞入OTHER||
|19|ERROR-COMMENT|錯誤|<優先顯示排列順序><br>1. 包含特殊字體: <br>`CONTAINS INVALID CHARACTERS.`<br>2. 輸入文字過長: <br>`ADDRESS DATA IS TOO LONG.`<br>3. OTHER 有值: <br>`PARSING FAILED. PLEASE CHECK INPUT.`<br>4. ZIP 為空值: <br>`PLEASE ENTER POSTAL CODE.`<br>5. COUNTRY 為空值: <br>`PLEASE ENTER COUNTRY.`<br>6. CITY 為空值: <br>`PLEASE ENTER CITY.`|※ 因SWIFT電文設計上，國家/郵遞區號/城市為必填欄位、不可包含特殊字體|
|20|CUSTOMER_ID|客戶 ID|-|**因未設定輸入方式，暫設為隨機序號**|
|23|ADDR_LINE_REBUILD|重組地址|<反結構><br>11[FLOOR] → 13[ROOM] → 14[BUILDING] → 9[NUMBER] → 8[ALLEY] → 7[LANE] → 6[SEC] → 5[SREET] → 18[OTHER] → 15[VILLAGE] →4[DISTRICT] → 3[CITY] → 16[PROVINCE] → 17[STATE] → 1[ZIP] → 2[COUNTRY]||
|其他分類標準|-|-|-||

***
### 詳細劃分規則
1. 荷蘭郵遞區號
  * 前半:4個數字
  * 後半:大寫英文*2
```
IF CNT(IDX) = TEMP-LEN
  IF TEMP-LEN = 4 AND
    NEXT-LEN = 2 AND
    FUNCTION TRIM(NEXT-COL) IS ALPHABETIC-UPPER AND
    (COUNTRY = "NETHERLANDS" OR COUNTRY = "NLD")
```

2. 英式郵遞區號
  * 英國
    * 前半：總字數1~2
    * 前半：總字數3~4，包含數字*1
    * 後半：包含數字*1、總字數3
  * 非英國
    * 前半：包含數字*1、總字數3
    * 後半：包含數字*1、總字數3~4  

標準寫法(有空格區分)("XXX XXX")
```
IF (
  *> 1. 非英國 標準寫法
  (
  (TEMP-LEN = 3                    AND CNT(IDX) = 1)) AND
  (NEXT-LEN >= 3 AND NEXT-LEN <= 4 AND CNT(IDX + 1) >= 1)

  OR

  *> 2. 英國 標準寫法
  ((COUNTRY = "UNITED KINGDOM" OR COUNTRY = "UK") AND
  ((TEMP-LEN >= 1 AND TEMP-LEN <= 2 AND CNT(IDX) < 2) OR
  (TEMP-LEN >= 3 AND TEMP-LEN <= 4 AND CNT(IDX) = 1)) AND
  (NEXT-LEN >= 3 AND NEXT-LEN <= 4 AND CNT(IDX + 1) = 1))

  OR

  *> 3. 荷蘭 標準寫法
  (COUNTRY = "NETHERLANDS" OR COUNTRY = "NLD" AND
  TEMP-LEN = 4 AND CNT(IDX) = 4 AND
  NEXT-LEN = 2 AND FUNCTION TRIM(NEXT-COL) IS ALPHABETIC-UPPER)

  )
```
手寫常見(無空格區分)("XXXXXX")
```
IF ( CNT-U(IDX) = "Y" AND (
  *> 1. 非英國 手寫常見
  (TEMP-LEN >= 6 AND TEMP-LEN <= 7 AND CNT(IDX) >= 2)

  OR

  *> 2. 英國 手寫常見
  ((COUNTRY = "UNITED KINGDOM" OR COUNTRY = "UK") AND
  ((TEMP-LEN >= 4 AND TEMP-LEN <= 6 AND CNT(IDX) < 3) OR
   (TEMP-LEN >= 6 AND TEMP-LEN <= 8 AND CNT(IDX) = 2)))
  )

  OR

  *> 3. 荷蘭 手寫常見  (會包含於1. 非英國 手寫常見中，此部分可省略)
  (COUNTRY = "NETHERLANDS" OR COUNTRY = "NLD" AND
  TEMP-LEN = 6 AND TEMP-PART(IDX)(1:4) IS NUMERIC AND
  FUNCTION TRIM(TEMP-PART(IDX)(5:2)) IS ALPHABETIC-UPPER)
  )
```

***
## 假資料/暫定資料
* 目前預設輸入地址資料為一整個完整地址
  * 是否需要調整成SWIFT電文形式?(1個地址分3?4?段輸入)
* 客戶統編資料
  * 目前於 `INPUT-ADDRESS.csv` 設定，`=ROUNDDOWN(RAND() * 10000000000; 0)`

***
## 爭議點?
* 輸入的地址格式
  * 國家必定會位於字尾
* FLOOR
  * `Lower Ground Floor`、`Mezzanine Floor`、`M/F`，是否需要變更簡短?
