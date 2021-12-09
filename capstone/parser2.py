f1 = open("myHerTree.ged", "r")
f2 = open("answer2.pl", "w")

base = {}
key = ""

for line in f1.readlines():
    words = line.split(" ")
    if len(words) >= 3:# рассматриваем только те строки где есть 3 слова "уровень" + "служебное" + "информация" + ...
        s1 = words[1]# служебное слово NAME, GIVN, SURN и тд
        s2 = words[2]# информация
        if s1[:2] == "@I":
            key = words[1]
        elif s1 == "NAME":
            name = words[2] + " " + words[3][:-1]# Убираем символ перевода строки у слова
            name = name.replace("'", "\\'")# Добавляем экранированный символ '\' перед d' - это ломает строки в прологе.
            name = name.replace("/", "")#очищаем имя от /
            base.update({key:name})
        elif s1 == "SEX":
            if s2[:-1] == "F":
                f2.write("female(\'%s\').\n" % (name))
            elif s2[:-1] == "M":
                f2.write("male(\'%s\').\n" % (name))
        elif s1 == "HUSB":
            for k, a in base.items():
                if k == words[2][:-1]:
                    father = a
        elif s1 == "WIFE":
            for k, a in base.items():
                if k == words[2][:-1]:
                    mother = a
        elif s1 == "CHIL":
            for k, a in base.items():
                if k == words[2][:-1]:
                    child = a
            f2.write("child('%s', '%s').\n" % (child, father))
            f2.write("child('%s', '%s').\n" % (child, mother)) 
f2.close()
f1.close()