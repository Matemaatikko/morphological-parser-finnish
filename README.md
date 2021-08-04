Morphological parser (finnish) 
==============================
(Suomenkielen morfologinen jäsennin)

Programming language: <b>Scala 3</b>

Project is based on wordlist from https://kaino.kotus.fi/sanat/nykysuomi/.

Files
=====
Kotus - wordlist
-----------------------
###Finnish:
For finnish explanation see: https://kaino.kotus.fi/sanat/nykysuomi/.

###English:
```
<s></s>     lemma 
<tn></tn>   bending class number
<av></av>   consonant gradation type
```

In nomines <b>lemma</b> is singular nominative form of a word. In some cases it can also be in plural form.
For verbs <b>lemma</b> is 1. Infinitive form of a word. 

<b>Bending class number</b> tells the bending class of a word. 
Example bendings for each bending classes can be found from [Rules](https://github.com/Matemaatikko/morphological-parser-finnish/tree/master/src/main/files/rules). 

````
1-49    nomines
50      compound word with prefix has no bending in cases 
51      compound word with prefix has bending in cases 
52-76   verbs
77-78   verbs with partial bending
99      no bending
101     pronoun
-       some compound words, adverbs and other words are missing class number
````

<b>Consonant gradation type </b> 
````
#letter #strong form:weak form
A       kk : k
B       pp : p
C       tt : t
D       k : –
E       p : v
F       t : d
G       nk : ng [ k :  ]
H       mp : mm
I       lt : ll
J       nt : nn
K       rt : rr
L       k : j
M       k : v
````

Updated kotus - wordlist
-----------------------
We reformat kotus-list to contain more information about bending. [Document](https://github.com/Matemaatikko/morphological-parser-finnish/blob/master/src/main/files/result/kotus_updated.txt).

````
paranthesis means that body is optional.
bending =  B:bending-class-number(:L:gradation-letter)
-------------
W:lemma:bending                              word
U:lemma                                      pronoun
N:lemma                                      no bending
P:prefix                                     prefix
E:-value                                     suffix with bending class parsing failed (will be treated as unbending)
E:lemma(:bending)                            word with bending class parsing failed (will be treated as unbending)
S:lemma:bending                              suffix
C1:lemma:P:prefix:S:suffix:bending           compound word with prefix having no bending in cases
C2:lemma:P:prefix:bending:S:suffix:bending   compound word with prefix having bending in cases
````

Bending - wordlist
-----------------------
Based on kotus - wordlist we generate list that maps words to lemma and bending information. 
[Documents](https://github.com/Matemaatikko/morphological-parser-finnish/blob/master/src/main/files/result/).


Consonant gradation rules
-------------------------


Incorrect bending (To be fixed)
=============================
- The following words have incorrect bending:
  - hapan
  - jälsi
  - viedä
- Some numerals have incorrect bending. Example: 'kahdeksan'

Missing properties (Upcoming)
=============================
- Bending of pronouns
- Possessive suffixes
- Comparation of adjectives
- Clitics (Liitepartikkelit)
- Nominal forms of verbs: declension

Copyright
=========
Published under: <b> Creative Commons NonCommercial license:
[Lisence](https://creativecommons.org/licenses/by-nc/4.0/legalcode). </b>

© Juho Salmensuu 2021