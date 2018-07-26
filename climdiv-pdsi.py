import csv

with open('C:\Users\jgcobb\Desktop\SESYNC\data\climdiv-pdsidv-v1-0-0-20180705.txt', 'r') as inf:
    with open('C:\Users\jgcobb\Desktop\SESYNC\data\climdiv-pdsidv.csv', 'wb') as outf:
        header = ['STATE-CODE','DIVISION-NUMBER','ELEMENT-CODE','YEAR','JAN-VALUE','FEB-VALUE','MAR-VALUE','APR-VALUE',\
                 'MAY-VALUE','JUNE-VALUE','JULY-VALUE','AUG-VALUE','SEPT-VALUE','OCT-VALUE','NOV-VALUE','DEC-VALUE']
        writer = csv.DictWriter(outf, fieldnames=header)
        writer.writeheader()
        for row in inf:
            record = {
                'STATE-CODE': row[0:2],
                'DIVISION-NUMBER': row[2:4],
                'ELEMENT-CODE': row[4:6],
                'YEAR': row[6:10],
                'JAN-VALUE': row[10:17].lstrip(),
                'FEB-VALUE': row[17:24].lstrip(),
                'MAR-VALUE': row[24:31].lstrip(),
                'APR-VALUE': row[31:38].lstrip(),
                'MAY-VALUE': row[38:45].lstrip(),
                'JUNE-VALUE': row[45:52].lstrip(),
                'JULY-VALUE': row[52:59].lstrip(),
                'AUG-VALUE': row[59:66].lstrip(),
                'SEPT-VALUE': row[66:73].lstrip(),
                'OCT-VALUE': row[73:80].lstrip(),
                'NOV-VALUE': row[80:87].lstrip(),
                'DEC-VALUE': row[87:94].strip(),
            }
            writer.writerow(record)