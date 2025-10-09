# 目前的課題：
* STATE需要判斷全名?目前僅判斷簡寫
* sa文件?
* 國家名 → 是否要追加 ISO 2位數的國家CODE（例：JP, US, TW）
* 新增多種測試地址
* 原文 + 英文


# Cobol-Test
## 執行方式
* 主程式: 收納於 src\main\
  * `EXECUTE.cob`
* 會被呼叫的程式: 收納於 src\modules\
  * `FORMATTER-ADDRESS.cob`
  * `OUTPUT-ADDRESS-CSV.cob`
  * `OUTPUT-ADDRESS.cob`
  * `READ-ADDRESS.cob`
  * `READ-RULE.cob`
  * `SPLIT-ADDRESS-FIELDS.cob`
* .exe檔: 收納於 bin\
```
PS C:\vs-workspaces\Cobol-Test> 
cobc -x -o bin\EXECUTE.exe src\main\EXECUTE.cob src\modules\READ-RULE.cob src\modules\READ-ADDRESS.cob src\modules\FORMATTER-ADDRESS.cob src\modules\SPLIT-ADDRESS-FIELDS.cob src\modules\OUTPUT-ADDRESS.cob src\modules\OUTPUT-ADDRESS-CSV.cob

.\bin\EXECUTE.exe
```

## 功能解說
* `EXECUTE.cob`
  * 呼叫`READ-RULE.cob`
    * 取得分類標準、國家名、城市名關鍵字清單
      * `CategoryRules.csv`: 分類標準
        * 預設上限: 40列(共210字) * 18行
        * 已做全大寫處理
        * 實際資料: 最多35列(路) * 18行，單欄最多12字
      * `CountryList.csv`: 國家清單(含簡寫)
        * 預設上限: 1列(50字) * 500行
        * 已做全大寫處理、**依照文字數多->少順序排列**、****
        * 實際資料: 共319列，單欄最多32字
      * `WorldCitiesList.csv`: 城市清單
        * 預設上限: 1列(50字) * 50000行
        * 已做全大寫處理、**依照文字數多->少順序排列**
        * 實際資料: 最多字欄位為49字，44280列

  * 呼叫`READ-ADDRESS.cob`
    * 取得需解讀之地址清單
      * 預設上限: 1列(共500字) * 1000行(INPUT-DATA)
    * 呼叫`FORMATTER-ADDRESS.cob`
      * 進行地址名稱的基本格式化
      * 移除多於空格
      * 盡可能插入逗號以利分段
    * 以逗號為依據，上限100字為上限，歸納至ADDRESS-LIST 5個欄位

  * 呼叫`SPLIT-ADDRESS-FIELDS.cob`
    * 對各筆地址進行欄位切分動作

  * 進行錯誤分類(僅顯示優先順序)
    1. 包含特殊字體: **CONTAINS INVALID CHARACTERS.**
    2. 輸入文字過長: **ADDRESS DATA IS TOO LONG.**
    3. OTHER 有值: **PARSING FAILED. PLEASE CHECK INPUT.**
    4. ZIP 為空值: **PLEASE ENTER POSTAL CODE.**
    5. COUNTRY 為空值: **PLEASE ENTER COUNTRY.**
    6. CITY 為空值: **PLEASE ENTER CITY.**  
    ※ 若ZIP/COUNTRY/CITY有複數個欄位空值，錯誤訊息將直接串聯顯示

  * 呼叫`OUTPUT-ADDRESS.cob`
    * 輸出所有結果:`Address_Split.txt`
      * 寫出: **TOTAL ITEMS: XXXX, ERROR ITEMS: YYYY**
    * 輸出錯誤結果清單:`Fail_Data.txt`
      * 若有錯誤資料，寫出: **ERROR ITEMS: YYYY**
      * 若無錯誤資料，寫出: **NO ERROR DATA**

  * 呼叫`OUTPUT-ADDRESS-CSV.cob`
    * 輸出所有結果:`Address_Split.txt`
      * 寫出: **TRESULT SUMMARY->OTAL ITEMS: XXXX, ERROR ITEMS: YYYY**
    * 輸出錯誤結果清單:`Fail_Data.txt`
      * 若有錯誤資料，寫出: **RESULT SUMMARY->ERROR ITEMS: YYYY**
      * 若無錯誤資料，寫出: **RESULT SUMMARY->NO ERROR DATA**
    * 使用EXCEL開啟`CSV`檔時之注意事項:
      1. 開啟空白活頁簿
      2. 資料 -> 從文字/CSV -> 匯入 -> 分隔符號:分號 -> 轉換資料
        * 標頭升階 -> 要有(使用第一個資料列作為標頭)
        * 已變更類型 -> 要拿掉
      3. 關閉並載入

## 分類規則
* FORMATTER-ADDRESS
  * 分區判斷處理
  1. 移除多餘空格
  2. "," -> ", "(後續需要)
  3. 使用`CountryList.csv`搜尋城市名稱
  4. 簡寫地名補上.
  5. 使用空格分割字串並進行主要分段判斷(參考下表)並插入","
  6. 微調分析結果
    * ",," -> ","
    * 分析結果若導致地址字首/字尾為","時將該字移除

* SPLIT-ADDRESS-FIELDS
  * 實際塞值進欄位
  1. 使用","分割字串並進行主要分段判斷(參考下表)，並將結果納入DTLS清單中
  2. 國家欄位判斷
  3. 進行初步判斷後，針對未判斷完成的字串進行特殊處理(OTHER CHECK)
  4. 特殊處理，針對確定進入OTHER的欄位再次判斷


| ADDRESS-DTLS<br>之位置 | 英文名| 中文名 |FORMATTER-ADDRESS|SPLIT-ADDRESS-FIELDS|備註|
|---|---|---|---|---|---|
|1|ZIP|郵遞區號|1. 荷蘭篩選<br> 2. 英式篩選<br> 3. 標準書寫/手寫常見<br>4. 全數字/"-"|1. 為數字或"-"所組成的字串<br>2.數字字串 + B<br>※ 相關判斷會先將值嘗試塞入NUMBER欄位，才流向郵遞區號<br>4. 英式篩選<br> 3. 標準書寫/手寫常見|[詳細劃分規則](#詳細劃分規則)|
|2|COUNTRY|國家|依照`CountryList.csv`內容尋找國家名稱，並納入`DTLS-LF`保管|-||   
|3|CITY|縣市|依照`WorldCitiesList.csv`內容尋找城市名稱，並於該城市名前後追加","<br>※ 若城市名後面連接"CITY"，視為同段字串<br>※ 若城市名後面連接方向(N/S/E/W等)，視為同段字串|≪OTHER CHECK≫<br>1. 若CITY欄位為空值，則優先移動OTHER內容至CITY<br>2. 若DISRICT為空值，則優先移動OTHER內容至CITY，並將舊CITY資料移動至DISTRICT<br>3. 若STREET為空值，則優先移動OTHER內容至CITY，並將舊CITY資料移動至STREET||
|4|DISTRICT|市區|依照`CategoryRules.csv`分類|依照`CategoryRules.csv`分類<br>≪OTHER CHECK≫<br>1. 若DISRICT為空值，則優先移動OTHER內容至CITY，並將舊CITY資料移動至DISTRICT||
|5|STREET|路|依照`CategoryRules.csv`分類|依照`CategoryRules.csv`分類<br>1. 若STREET為空值，則優先移動OTHER內容至CITY，並將舊CITY資料移動至STREET||
|6|SEC|段|依照`CategoryRules.csv`分類|依照`CategoryRules.csv`分類||
|7|LANE|巷|依照`CategoryRules.csv`分類|依照`CategoryRules.csv`分類||
|8|ALLEY|弄|依照`CategoryRules.csv`分類|依照`CategoryRules.csv`分類||
|9<br>~~10~~|M-NO<br>~~S-NO~~|號|依照`CategoryRules.csv`分類<br>1. 為數字或"-"所組成的字串<br>2.數字字串 + B<br>3.數字字串 + 序數詞(st/nd/rd/th)<br>※ 若數字字串後面連接數字字串 + 序數詞，視為同段字串|依照`CategoryRules.csv`分類<br>1. 為數字或"-"所組成的字串<br>2.數字字串 + B<br>※ 相關判斷會先將值嘗試塞入NUMBER欄位，才流向郵遞區號|原為考量x號之y分成2段，目前改不考慮的緣故10號不使用|
|11<br>~~12~~|M-FLOOR<br>~~S-FLOOR~~|樓|依照`CategoryRules.csv`分類<br>xxx + F.<br>xxx + FL.<br>xxx + FLOOR<br>之字串|依照`CategoryRules.csv`分類|原為考量x樓之y分成2段，目前改不考慮的緣故12號不使用|
|13|ROOM|室|依照`CategoryRules.csv`分類|依照`CategoryRules.csv`分類||
|14|BUILDING|建築大樓|依照`CategoryRules.csv`分類|依照`CategoryRules.csv`分類||
|15|VILLAGE|社區|依照`CategoryRules.csv`分類|依照`CategoryRules.csv`分類||
|16|PROVINCE|省份|依照`CategoryRules.csv`分類|依照`CategoryRules.csv`分類||
|17|STATE|州|1.字數2~3<br>2.皆為大寫|1.字數2~3<br>2.皆為大寫||
|18|OTHER-COL|其他|-|≪OTHER CHECK≫<br>若字串未成功塞入任何欄位，則強制塞入OTHER||
|19|ERROR-COMMENT|錯誤|`READ-ADDRESS.cob`<br>1. 文字超過上限(某欄位超過35字):<br>`ADDRESS DATA IS TOO LONG.`<br>2. 包含特殊字體:<br>`CONTAINS INVALID CHARACTERS.`|`EXECUTE.cob`<br>1. ZIP 為空值<br>2. COUNTRY 為空值<br>3. CITY 為空值<br>`PLEASE ENTER POSTAL CODE COUNTRY CITY.`<br>4. OTHER 有值:<br>`PARSING FAILED. PLEASE CHECK INPUT.`|※ 因SWIFT電文設計上，國家/郵遞區號/城市為必填欄位、不可包含特殊字體|
|20|CUSTOMER_ID|客戶 ID|-|-|**因未設定輸入方式，暫設為流水號**|
|-|其他分類標準|-|1. 若字串後方有縮寫符號(.)/換欄符號(,)，視為需分段字串||

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

## 假資料/暫定資料
* 輸入地址資料為一整個完整地址
  * 是否需要調整成SWIFT電文形式?(1個地址分3?4?段輸入)
* 客戶統編資料
  * 目前於 `EXECUTE` 設定，並依照處理順序安插序號(MOVE JDX TO CUSTOMER_ID(JDX))(15字)

## 爭議點?
* FLOOR
  * `Lower Ground Floor`、`Mezzanine Floor`、`M/F`，是否需要變更簡短?