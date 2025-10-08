       01  ADDRESS-DATA.
           03 ADDRESS-COLUMNS OCCURS 1000 TIMES.
              05  ADDRESS-FULLNAME PIC X(2000).
              05  ADDRESS-DTLS     PIC X(100) OCCURS 20 TIMES.
              05  ZIP              PIC X(35).  *> 1 郵遞區號
              05  COUNTRY          PIC X(100). *> 2 國家
              05  CITY             PIC X(100). *> 3 縣市
              05  DISTRICT         PIC X(100). *> 4 市區
              05  STREET           PIC X(100). *> 5 路
              05  SEC              PIC X(35).  *> 6 段
              05  LANE             PIC X(50).  *> 7 巷
              05  ALLEY            PIC X(35).  *> 8 弄
              05  M-NO             PIC X(35).  *> 9 號
              05  S-NO             PIC X(35).  *> 10 號
              05  M-FLOOR          PIC X(35).  *> 11 樓
              05  S-FLOOR          PIC X(35).  *> 12 樓
              05  ROOM             PIC X(35).  *> 13 室
              05  BUILDING         PIC X(100). *> 14 建築大樓
              05  VILLAGE          PIC X(100). *> 15 村里
              05  PROVINCE         PIC X(35).  *> 16 省份
              05  STATE            PIC X(100). *> 17 州
              05  OTHER-COL        PIC X(100). *> 18 其他

              05  ERROR-COMMENT    PIC X(40).  *> 19 錯誤
              05  CUSTOMER_ID      PIC X(15).  *> 客戶 ID
       
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
