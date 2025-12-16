目前の課題：
* ST.はCITYの場合もある（St. George's）
* DE LA
* sa文件
* チュートリアルの補完

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

* 產出檔案為`.CSV`檔3份及`.txt`檔各2份(總資料分析結果/ 分析失敗地址清單/ 執行LOG)
  * 使用EXCEL開啟`.CSV`檔時之注意事項:
    1. 開啟空白活頁簿
    2. 資料 -> 從文字/CSV -> 匯入 -> 分隔符號:分號 -> 轉換資料
      * 標頭升階 -> 要有(使用第一個資料列作為標頭)
      * 已變更類型 -> 要拿掉
    3. 關閉並載入

***
## 功能解說
```
EXECUTE.cob: 主程式
  ├─ MAIN SECTION: 若測試名稱為"TT"開頭字串，執行 PERFORM-ADDRESS-WORKFLOW 10次
  │                若測試名稱為"QQ"開頭字串或(空欄)，執行 1次 但不紀錄 OUTPUT_LOG.csv
  └─ PERFORM-ADDRESS-WORKFLOW: 
      ├─ READ-RULE.cob : 讀取分類規則/ 判斷對象
      │  ├─ 基本設定1  : 方向關鍵字
      │  ├─ 基本設定2  : 特殊關鍵字
      │  ├─ 輸入資料1  : CategoryRules.csv
      │  ├─ 輸入資料2  : CountryList.csv
      │  └─ 輸入資料3  : StateFullnameList.csv
      ├─ 外部資料4     : INPUT-ADDRESS.csv
      ├─ FORMATTER-ADDRESS.cob: 分析地址
      │  └─ MAIN SECTION
      │      ├─ 1. FA-DATA-FORMATTING: 整頓原始資料
      │      ├─ 2. FA-EXTRACT-COUNTRY: 抽出 國家
      │      ├─ 3. FA-EXTRACT-STATE  : 抽出 STATE
      │      ├─ 4. FA-EXTRACT-EW     : 抽出 EXCEPTION-WORD
      │      ├─ 5. FA-COUNT-CNT      : 計算各欄位字數 數字判斷
      │      ├─ 6. FA-SPLIT-BY-LOGIC : 依照 分割規則 拆解地址
      │      ├─ 7. FA-REBUILD        : 反結構
      │      └─ 8. FA-ERROR-SECTION  : 處理錯誤資料
      ├─ OUTPUT-ADDRESS.cob: 整理輸出資料字串
      ├─ 輸出資料1: Address_Split.csv
      ├─ 輸出資料2: Fail_Data.csv
      ├─ 輸出資料3: Address_Split.txt
      ├─ 輸出資料4: Fail_Data.txt
      └─ 輸出資料5: OUTPUT_LOG.csv
```

* `EXECUTE.cob`
  * 輸入測試資料並隨筆記錄時間(總執行時間/ 開始執行日期和時間)

  * 呼叫`READ-RULE.cob`: 取得分類標準、國家名、城市名關鍵字清單
    * **※ 登錄關鍵字時，請將所有關鍵字進行轉大寫處理後再登錄**
      * `DIR-NAMES`、`DIR-LEN`: 方向關鍵字(STREET判斷使用)
        * 資料欄位: `方向關鍵字名稱`、`關鍵字總數`
        * 程式變數: `DIR-NAMES`、`DIR-LEN`
        * 預設上限: 23個
        * 實際資料: 23個
        * ※ 追加關鍵字時，所有變數需同時新增並更新
      * `EXCEPTION-WORD-TABLE`: 例外處理字典
        * 資料欄位: `關鍵字名稱`、`關鍵字種類`、`會使用該關鍵字之國家`、`關鍵字總數`
        * 程式變數: `EXCEPTION-WORD`、`EXCEPTION-FLAG`、`EXCEPTION-COUNTRY`、`EXCEPTION-LEN`
        * 預設上限: 10個
        * 實際資料: 3個
        * ※ 追加關鍵字時，所有變數需同時新增並更新
        * ※ "國家"若為空欄，表示該關鍵字的使用不限國家
      * `CategoryRules.csv`: 分類標準
        * 程式變數: `LS-LIST-G`(18) -> `LS-LIST-COL`(40)
        * 預設上限: 50列(共270字) * 18行
        * 實際資料: 最多45欄(STREET) * 18行，單欄最多14字，單行最長256字
      * `CountryList.csv`: 國家清單(含ISO簡寫)
        * 資料欄位: `國家名稱`、`ISO代號(2碼)`
        * 程式變數: `LS-COUNTRY-NAME`、`LS-COUNTRY-CODE`
        * 預設上限: 2列(50字) * 500行
        * 實際資料: 共385列，單欄最多43字
        * ※ 追加關鍵字時，**依照文字數多->少順序排列**
        * ※ 可為預防表記搖擺狀況，同一ISO重複登錄
      * `StateFullnameList.csv`: 州名稱清單(簡寫)
        * 資料欄位: `州名稱(全名)`、`代號(2~3碼)`、`國家(ISO)`
        * 程式變數: `LS-STATE-NAME`、`LS-STATE-CODE`、`LS-STATE-COUNTRY`
        * 預設上限: 3列(各40字，總60字) * 250行
        * 實際資料: 最多字欄位為49字，205列
        * ※ 追加關鍵字時，**依照國家->文字數多->少順序排列**

  * `INPUT-ADDRESS.csv`: 需解讀之地址清單
    * 資料欄位: `CIFKEY`、`中文地址`、`英文地址`
    * 記錄變數: `IF-DATA`
    * 預設上限: 1列(共500字)(IN-FILE-REC) * 行數無限制(讀完/讀取內容為空白)
      * 2-30列: 第二批測試資料
      * 31-150列: 第一批測試資料
      * 151-162列: 樓層測試資料
      * 163-175列: 第二批測試資料
      * 176-196列: STATE測試資料

  * 呼叫`FORMATTER-ADDRESS.cob`: 分析地址
    1. `FA-DATA-FORMATTING`: 整頓原始資料
      * 移除特殊字
        * 以`ADD.`為首的字串，移除該字串
        * 以`ON`為首的字串，移除該字串
        * 若包含`C.A.P.`字串，移除該字串
      * 整頓標點符號
        * 字首`,`
        * 字尾`,`
        * 移除重複`,,`，`,,` -> `,`
        * `,X` -> `, X`(後續需要依空白為分割關鍵字)
        * `.,X` -> `., X`
        * `X.9` -> `X. 9`
      * 移除多餘空格
      * 若找到特殊關鍵字將該字串後端補上`.`
        * 2文字: `ST`、`RD`、`DR`、`NO`、`LN`
        * 3文字: `AVE`、`RIV`、`ALY`、`LTD`
        * 4文字: `BLDG`、`BLVD`、`DIST`、`DEPT`
      * `P.O. BOX` -> `P.O.-BOX`
    2. `FA-DATA-FORMATTING`: 抽出 國家
      * 使用`CountryList.csv`搜尋國家名稱(全名/ISO)，並轉換成ISO國家代碼
      * 若分析對象中出現複數個國家關鍵字，只取最右端字串為對象
    3. `FA-EXTRACT-STATE`: 抽出 STATE
      * 使用`StateFullnameList.csv`搜尋州名(全名/簡寫)，並轉換成簡寫
    4. `FA-EXTRACT-EW`: 抽出 EXCEPTION-WORD
      * 使用`EXCEPTION-WORD-TABLE`內容抽出登錄至例外辭典的詞彙
    5. `FA-COUNT-CNT`: 計算各欄位字數 數字判斷(若有找到才填入下列資訊)
      * 依空格分欄判斷各字串(`,`不列入計算範圍)
        * `PC-CNT-NUM`: 記錄數字/ `-`字數
        * `PC-CNT-CHAR`: 記錄數字/ `-`字數
        * 尋找是否包含 LS-LIST-COL 關鍵字
          1. `PC-CNT-KEY-I`: 於出現關鍵字之字串位置記錄關鍵字之種類
            * 若為關鍵字: 填入 `種類代號`
            * 若為羅馬關鍵字(-XXX): 填入 `種類代號 + 30`
            * 若為後接關鍵字(XXX-): 填入 `種類代號 + 60`
          2. `PC-CNT-KEY-W`: 記錄出現之關鍵字
        * 尋找是否包含 DIR-NAMES 方向關鍵字
          1. `PC-CNT-KEY-I`: 於出現關鍵字之字串位置記錄關鍵字之種類(固定99)
      * 若找到結尾位置，則預設該位置塞入欄位至國家(PC-PART-CHECK(IDX) = 2)，若STATE有值，則記錄PC-OTHER-STATE非0
    6. `FA-SPLIT-BY-LOGIC`: 依照 [分割規則](#分類規則) 拆解地址
    7. `FA-REBUILD`: 反結構
      * 依照分類完成之內容組回一串地址
        1. 10[DEPARTMENT]
        2. 18[SUB-DEPARTMENT]
        3. 13[ROOM]
        4. 11[FLOOR]，若該欄內容不包含`F`、`LEVEL`(為純數字)，則追加`FLOOR`綴字
        5. 14[BUILDING]
        6. 9[NUMBER]
        7. 8[ALLEY]，若該欄內容為純數字，則追加`ALY.`綴字
        8. 7[LANE]，若該欄內容為純數字，則追加`LN.`綴字
        9. 6[SEC]，若該欄內容為純數字，則追加`SEC.`綴字
        10. 5[SREET]
        11. 15[VILLAGE]
        12. 24[OTHER]
        13. 12[POST-BOX]
        14. 4[DISTRICT]
        15. 3[CITY]
        16. 16[PROVINCE]
        17. 17[STATE]
        18. 1[ZIP]
        19. 2[COUNTRY]
      * 串接各字串時，各字串間補上`, `
    8. `FA-ERROR-SECTION`: 處理錯誤資料
      * 進行錯誤分類(僅顯示優先順序高之項目)，下列優先度: 1 > 6
        1. 包含特殊字體: **CONTAINS INVALID CHARACTERS.**
        2. 輸入文字過長: **ADDRESS DATA IS TOO LONG.**
        3. OTHER 有值: **PARSING FAILED. PLEASE CHECK INPUT.**
        4. ZIP 為空值: **PLEASE ENTER POSTAL CODE/ / POST BOX.**
        5. COUNTRY 為空值: **PLEASE ENTER COUNTRY.**
        6. CITY 或 PROVINCE 為空值: **PLEASE ENTER CITY OR PROVINCE.**  
        * ※ 若ZIP/COUNTRY/(CITY/PROVINCE)有複數個欄位空值，錯誤訊息將直接串聯顯示

  * 呼叫`OUTPUT-ADDRESS.cob`
    * 整理輸出結果內容

  * TXT檔
    * 輸出所有結果:`Address_Split.txt`
      * 寫出: **TOTAL ITEMS: XXXX, ERROR ITEMS: YYYY**
    * 輸出錯誤結果清單:`Fail_Data.txt`
      * 若有錯誤資料，寫出: **ERROR ITEMS: YYYY**
      * 若無錯誤資料，寫出: **NO ERROR DATA**

  * CSV檔
    * 輸出所有結果:`Address_Split.csv`
      * 寫出: **TRESULT SUMMARY->OTAL ITEMS: XXXX, ERROR ITEMS: YYYY**
    * 輸出錯誤結果清單:`Fail_Data.csv`
      * 若有錯誤資料，寫出: **RESULT SUMMARY->ERROR ITEMS: YYYY**
      * 若無錯誤資料，寫出: **RESULT SUMMARY->NO ERROR DATA**

  * 輸出所有執行時所費時間之結果:`OUTPUT_LOG.csv`
    * TEST-NEME: 測試名稱
    * TIME: 測試執行時間(原CODE)
    * TIME(HH:MM:SS.CC)
    * DATE: 執行日期
    * TIME: 執行時間(UTC+8)
    * COUNT: 測試資料之筆數
    * AVG: 1資料處理平均時間(TIME/ COUNT)
      * ※ 若 TEST-NEME 輸入"TT"開頭之字串，相同測試本次資料`執行10次`
      * ※ 若 TEST-NEME 輸入"QQ"開頭之字串或(空白)，本次執行`不會記錄資料`
      * ※ TEST-NEME 請以英文輸入(否則有可能會亂碼)

***
## 分類規則
* FORMATTER-ADDRESS、`FA-SPLIT-BY-LOGIC`
  * 分區判斷處理，分析內容納入`BEFORE-DATA`


| DTLS-LF<br>之位置 | 英文名| 中文名 |FORMATTER-ADDRESS|備註|
|---|---|---|---|---|
|1|ZIP|郵遞區號|1. 荷蘭篩選<br> 2. 英式篩選<br> 3. 標準書寫/手寫常見<br>4. 為純數字或"-"所組成的字串(此條件優先度NUMBER > ZIP/ 此欄位+1/2格為國家)<br>5. 若國家為 摩納哥"MC" 或 丹麥"DK" 且 數字字數 <= 2|[詳細劃分規則](#詳細劃分規則)|
|2|COUNTRY|國家|依照`CountryList.csv`內容尋找國家名稱|-|   
|3|CITY|縣市|1. 依照`CategoryRules.csv`分類<br>2. 判斷對象之最右方之文字串(非COUNTRY/ STATE/ 郵遞區號)|-|
|4|DISTRICT|市區|依照`CategoryRules.csv`分類<br>≪OTHER CHECK≫<br>1. 若DISRICT為空值，則優先移動OTHER內容至CITY，並將舊CITY資料移動至DISTRICT|-|
|5|STREET|路|1. 依照`CategoryRules.csv`分類<br>2. "{非關鍵字字串/VIA} {方向關鍵字/LOOP/DE/DEL} {關鍵字字串}"<br>3. 若 STREET 欄位為空值 且 非倒數第1~3位則視為STREET|※ 若判斷對象為`IT`(義大利)，則取至有`,`、數字、已被分類欄位<br>※ 當判斷中欄位和下一個判斷欄位皆包含STREET關鍵字時，跳過目前判斷中對象|
|6|SEC|段|依照`CategoryRules.csv`分類|~~`欄位中省略關鍵字`~~ <br>為保持資料完整性，目前已取消省略動作|
|7|LANE|巷|依照`CategoryRules.csv`分類|~~`欄位中省略關鍵字`~~ <br>為保持資料完整性，目前已取消省略動作|
|8|ALLEY|弄|依照`CategoryRules.csv`分類|~~`欄位中省略關鍵字`~~ <br>為保持資料完整性，目前已取消省略動作|
|9|M-NO|號|依照`CategoryRules.csv`分類<br>1. 為純數字或"-"所組成的字串(此條件優先度NUMBER > ZIP)<br>2.{數字字串} * 1以上 + {大寫英文字} * 1<br>3. 若判斷郵遞區號時國家並非 摩納哥"MC" 或 丹麥"DK" 且 數字字數 <= 2|~~`欄位中省略關鍵字`~~ <br>為保持資料完整性，目前已取消省略動作<br>若判斷中對象欲移動之至 NUMBER(9)但已有值，則將判斷中內容移動至 FLOOR(11)(數字)/ BUILDING(14)<br>為保持資料完整性，目前已取消省略動作|
|10|DEPARTMENT|部門|依照`CategoryRules.csv`分類|若判斷中對象欲移動之至 部門(10)但已有值，則將判斷中內容移動至 子部門(18)|
|11|M-FLOOR|樓|1. 依照`CategoryRules.csv`分類<br>2. `{數字} F.`/ `{數字} FL.`/ `{數字}F`/ `B{數字}` 之字串<br>3. 數字字串 + 序數詞(st/nd/rd/th)|~~`欄位中省略關鍵字`~~ <br>為保持資料完整性，目前已取消省略動作|
|12|POST-BOX|POST-BOX|依照`CategoryRules.csv`分類|-|
|13|ROOM|室|依照`CategoryRules.csv`分類|-|
|14|BUILDING|建築大樓|1. 依照`CategoryRules.csv`分類<br>2.若前一欄位塞值入FLOOR|若判斷中對象欲移動之至 BUILDING(14)但已有值，則將判斷中內容移動至元欄位內容後方串聯|
|15|VILLAGE|社區|依照`CategoryRules.csv`分類<br>1.若前一欄位塞值入STREET|-|
|16|PROVINCE|省份|依照`CategoryRules.csv`分類|-|
|17|STATE|州|1.字數2~3<br>2.皆為大寫<br>3.依照`StateFullnameList.csv`分類<br>|-|
|18|SUB-DEPARTMENT|子部門|-|若字串未成功塞入任何欄位，則強制塞入OTHER|若判斷中對象欲移動之至 子部門(18)但已有值，則將判斷中內容移動至 子部門(10)|
|19|ERROR-COMMENT|錯誤|<優先顯示排列順序><br>1. 包含特殊字體: <br>`CONTAINS INVALID CHARACTERS.`<br>2. 輸入文字過長: <br>`ADDRESS DATA IS TOO LONG.`<br>3. OTHER 有值: <br>`PARSING FAILED. PLEASE CHECK INPUT.`<br>4. ZIP 為空值: <br>`PLEASE ENTER POSTAL CODE.`<br>5. COUNTRY 為空值: <br>`PLEASE ENTER COUNTRY.`<br>6. CITY 或 PROVINCE 為空值: <br>`PLEASE ENTER CITY OR PROVINCE.`|※ 因SWIFT電文設計上，國家/郵遞區號/城市為必填欄位、不可包含特殊字體|
|20|CIFKEY|客戶 ID|-|**因未設定輸入方式，暫設為隨機序號**|
|23|ADDR_LINE_REBUILD|重組地址|<反結構><br>10[DEPARTMENT] -> 18[SUB-DEPARTMENT] -> 13[ROOM] → 11[FLOOR] → 14[BUILDING] → 9[NUMBER] → 8[ALLEY] → 7[LANE] → 6[SEC] → 5[SREET] → 15[VILLAGE] → 24[OTHER] → 12[POST-BOX] → 4[DISTRICT] → 3[CITY] → 16[PROVINCE] → 17[STATE] → 1[ZIP] → 2[COUNTRY]|-|
|24|OTHER-COL|其他|-|-|
|其他分類標準1|EXCEPTION-WORD-TABLE|例外字典|判斷特殊字串(例:TAIPEI 101若不登錄，則會被判斷成郵遞區號101台北市)|-|

***
### 詳細劃分規則
* `一般例（数字のみ）`

| 国名 | ISO | 郵便番号のけた数、文字の種類 |
| --- | --- | --- |
| アイスランド | IS | 数字3けた　例：110 |
| オーストラリア | AU | 数字4けた　例：2060 |
| オーストリア | AT | 数字4けた　例：1120 |
| ブルガリア | BG | 数字4けた　例：1278 |
| リヒテンシュタイン | LI | 数字4けた　例：9485 |
| ベルギー | BE | 数字4けた　例：1050 |
| スイス | CH | 数字4けた　例：3008 |
| スロベニア | SI | 数字4けた　例：1000 |
| デンマーク | DK | 数字4けた　例：2300 |
| ノルウェー | NO | 数字4けた　例：0352 |
| ハンガリー | HU | 数字4けた　例：1011 |
| イタリア | IT | 数字5けた　例：00184 |
| エストニア | EE | 数字5けた　例：69501 |
| 韓国 | KR | 数字5けた　例：01000 |
| スペイン | ES | 数字5けた　例：28021 |
| フランス | FR | 数字5けた　例：75001 |
| ドイツ | DE | 数字5けた　例：10115 |
| ギリシャ | GR | 数字3+2けた　例：104 32 |
| スロバキア | SK | 数字3+2けた　例：810 00 |
| ポーランド | PL | 数字2+3けた　例：02-502 |
| チェコ | CZ | 数字3+2けた　例：160 00 |
| シンガポール | SG | 数字6けた　例：546080 |
| 中国 | CN | 数字6けた　例：853012 |
| ルーマニア | RO | 数字6けた　例：013696 |
| ポルトガル | PT | 数字4+3けた　例：1300-016 |
| 米国 | US | 数字5＋4けた　例：20001-1234 |

* `特殊例（パターン固定）`

| 国名 | ISO | 郵便番号のけた数、文字の種類 |
| --- | --- | --- |
| カナダ | CA | 3番目と4番目の文字の間のスペースで、6文字の英数字　例：H3Z 2Y7 |
| オランダ | NL | 数字4けた+アルファベット2けた 例：1071 DJ |
| 英国 | GB | 5～7けたの英数字 例：E4 9RT、CR0 3RL、EC1Y 8SY |
| マルタ | MT | アルファベット3けた+数字4けた 例：RBT 6023 |
| アイルランド | IE | 7けたの英数字（Eircodeと呼ばれるコード） 例：T37 F8HK |
| バミューダ | BM | 英文字２桁　数字2桁　例：FL 07 |
| アメリカ領ヴァージン諸島 | VI | 英文字２桁+数字5けた |
| ルクセンブルク | LU | 「L」 + 数字4けた　例：L-1234 |
| アンドラ | AD | 「AD」 + 数字4けた　例：AD1234 |
| アゼルバイジャン | AZ | 「AZ」 + 数字4けた　例：AZ 1000 |
| ブルネイ | BN | 「BT」 + 数字4けた　例：BT2328 |
| キプロス | CY | 「CY」 + 数字4けた　例：CY-2008 |
| ハイチ | HT | 「HT」 + 数字4けた　例：HT 6120 |
| セントルシア | LC | 「LC」 + 数字4けた　例：LC04 101 |
| ラトビア | LV | 「LV」 + 数字4けた　例：LV-1000 |
| モルドバ | MD | 「MD」 + 数字4けた　例：MD-2000 |
| スロベニア | SI | 「SI」 + 数字4けた　例：SI-4000 |
| サモア | WS | 「WS」 + 数字4けた　例：WS1251 |
| バルバドス | BB | 「BB」 + 数字5けた　例：BB25001 |
| キューバ | CU | 「CP」 + 数字5けた　例：CP 10600 |
| フィンランド | FI | 「FI」 + 数字5けた　例：FI-00100 |
| ミクロネシア | FM | 「FM」 + 数字5けた　例：FM 96941 |
| グアム | GU | 「GU」 + 数字5けた　例：GU-96910 |
| クロアチア | HR | 「HR」 + 数字5けた　例：HR-10000 |
| ソマリア | SO | 「JH」 + 数字5けた　例：JH 09010 |
| リトアニア | LT | 「LT」 + 数字5けた　例：LT-04340 |
| マーシャル諸島 | MH | 「MH」 + 数字5けた　例：MH 96960 |
| プエルトリコ | PR | 「PR」 + 数字5けた　例：PR-00601 |
| パラオ | PW | 「PW」 + 数字5けた　例：PW-96940 |
| スウェーデン | SE | 「SE」 + 数字3+2けた　例：SE-111 81 |
| アフリカ地域 | AF | 郵便番号はないが、P.O. BOXはある 例：03 BP 1000 |
| カザフスタン | KZ | 任意7けた 例：Z00Y5M3 |
| アルゼンチン | AR | 英数字8文字（1文字、4桁と3文字） 例：B1636FDA |
| ガーナ | GH | 英文字2けた、数字３けた、数字４けた（グリッド範囲）　例：EN-200-1987 |
| ナウル | NR | NRU68 |
| セントヘレナ | SH | STHL 1ZZ |
| タークス・カイコス諸島 | TC | TKCA 1ZZ |
| フォークランド諸島 | FK | FIQQ 1ZZ |


## 各國差異筆記
* 參考資料:
  1. [郵便番号 | ゆうびんばんご](https://ja.m.youbianku.com/?utm_source=chatgpt.com)
  2. [海外の郵便番号 - 日本郵便](https://www.post.japanpost.jp/int/zipcode/index.html)
* 英式等郵遞區號差異
* 新加坡(SG) 英文撰寫順序 [COUNTRY] -> [ZIP]
* 澳門 香港沒有郵遞區號

## 假資料/暫定資料
* 目前預設輸入地址資料為一整個完整地址(會包含國家、郵遞區號、TOWNNAME、ADDRESS_LINE的一串地址)
* 客戶統編資料
  * 目前於 `INPUT-ADDRESS.csv` 設定，`=ROUNDDOWN(RAND() * 10000000000; 0)`

***
## 爭議點?
* 是否需要調整成SWIFT電文形式?(1個地址分3?4?段輸入)
* 輸入的地址格式
  * STATE名與CITY名撞到時，會被優先填入STATE中
* FLOOR
  * `Lower Ground Floor`、`Mezzanine Floor`、`M/F`，是否需要變更簡短?
