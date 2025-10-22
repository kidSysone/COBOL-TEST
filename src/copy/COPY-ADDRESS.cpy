      *01  ADDRESS-DATA.
      *       05  ZIP              PIC X(35).  *> 1 郵遞區號
      *       05  COUNTRY          PIC X(100). *> 2 國家
      *       05  CITY             PIC X(100). *> 3 縣市
      *       05  DISTRICT         PIC X(100). *> 4 市區
      *       05  STREET           PIC X(100). *> 5 路
      *       05  SEC              PIC X(35).  *> 6 段
      *       05  LANE             PIC X(50).  *> 7 巷
      *       05  ALLEY            PIC X(35).  *> 8 弄
      *       05  M-NO             PIC X(35).  *> 9 號
      *       05  S-NO             PIC X(35).  *> 10 號
      *       05  M-FLOOR          PIC X(35).  *> 11 樓
      *       05  S-FLOOR          PIC X(35).  *> 12 樓
      *       05  ROOM             PIC X(35).  *> 13 室
      *       05  BUILDING         PIC X(100). *> 14 建築大樓
      *       05  VILLAGE          PIC X(100). *> 15 村里
      *       05  PROVINCE         PIC X(35).  *> 16 省份
      *       05  STATE            PIC X(100). *> 17 州
      *       05  OTHER-COL        PIC X(100). *> 18 其他
      *
      *       05  ERROR-COMMENT    PIC X(40).  *> 19 錯誤
      *       05  CUSTOMER_ID      PIC X(15).  *> 20 客戶 ID
      *       05  ADDR_LINE_ORIG   PIC X(150). *> 21 讀取_原文
      *       05  ADDR_LINE_EN     PIC X(100). *> 22 讀取_英文 
      *       05  ADDR_LINE_REBUILD     PIC X(100). *> 23 重組地址
       
      *    ZIP
      *    COUNTRY
      *    CITY
      *    DISTRICT
      *    STREET
      *    SEC
      *    LANE
      *    ALLEY
      *    NUMBER
      *    FLOOR
      *    ROOM
      *    BUILDING
      *    VILLAGE
      *    PROVINCE
      *    STATE
      *    OTHER
      *    
      *    ERROR_MESSAGE
      *    CUSTOMER_ID

       01  CBPRPLUS.
              05  DEPARTMENT            PIC X(70).  *> 1 部門
              05  SUB-DEPARTMENT        PIC X(70).  *> 2 子部門
              05  STREET-NAME           PIC X(70).  *> 3 街道名稱
              05  BUILDING-NUMBER       PIC X(16).  *> 4 大樓號碼(建築物編號)
              05  BUILDING-NAME         PIC X(35).  *> 5 大樓(建築物)名稱
              05  FLOOR                 PIC X(70).  *> 6 樓層
              05  POST-BOX              PIC X(16).  *> 7 郵政信箱
              05  ROOM                  PIC X(70).  *> 8 室
              05  POST-CODE             PIC X(16).  *> 9 郵遞區號
              05  TOWN-NAME             PIC X(35).  *> 10 鄉、鎮、市
              05  TOWN-LOCATION-NAME    PIC X(35).  *> 11 (鄉、鎮、市)所在地
              05  DISTRICT-NAME         PIC X(35).  *> 12 區名
              05  COUNTRY-SUB-DIVISION  PIC X(35).  *> 13 國家之次級行政區
              05  COUNTRY               PIC X(2).   *> 14 國家
              05  ADDRESS-LINE          PIC X(70).  *> 15 Address line

              05  OTHER-COL        PIC X(100). *> 18 其他
              05  ERROR-COMMENT    PIC X(40).  *> 19 錯誤
              05  CUSTOMER_ID      PIC X(15).  *> 20 客戶 ID
              05  ADDR_LINE_ORIG   PIC X(150). *> 21 讀取_原文
              05  ADDR_LINE_EN     PIC X(100). *> 22 讀取_英文 
              05  ADDR_LINE_REBUILD     PIC X(100). *> 23 重組地址
