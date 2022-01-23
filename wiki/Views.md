## Contents
- [Views](#views)
	- [Introduction](#introduction)
	- [Nível 1](#markdown-header-nivel-1)
		- [Vista Lógica](#N1-VL)
		- [Vista de Processos](#vista-de-processos)
               - [SSD UC03](#SSD-UC03)
               - [SSD US05](#SSD-US05)
               - [SSD UC06](#SSD-UC06)
               - [SSD UC07](#SSD-UC07)
               - [SSD UC08](#SSD-UC08)
               - [SSD US09](#SSD-US09)
               - [SSD UC10](#SSD-UC10)
               - [SSD UC11](#SSD-UC11)
               - [SSD UC12](#SSD-UC12)
               - [SSD UC33](#SSD-UC33)
               - [SSD UC35](#SSD-UC35)
               - [SSD UC13](#SSD-UC13)
	- [Nível 2](#markdown-header-nivel-2)
		- [Vista Lógica](#N2-VL)
		- [Vista de Processos](#vista-de-processos-1)
               - [SSD UC03](#SSD-UC03-(N2))
               - [SSD US05](#SSD-US05-(N2))
               - [SSD UC06](#SSD-UC06-(N2))
               - [SSD UC07](#SSD-UC07-(N2))
               - [SSD UC08](#SSD-UC08-(N2))
               - [SSD US09](#SSD-US09-(N2))
               - [SSD UC10](#SSD-UC10-(N2))
               - [SSD UC12](#SSD-UC11-(N2))
               - [SSD UC12](#SSD-UC12-(N2))
               - [SSD UC33](#SSD-UC33-(N2))
               - [SSD UC35](#SSD-UC35-(N2))
               - [SSD UC13](#SSD-UC13-(N2))
          - [(outros SSD arquiteturalmente relevantes)](#outros-ssd-arquiteturalmente-relevantes-1)
		- [Vista de Implementação](#N2-VI)
		- [Vista Física](#vista-física)
	- [Nível 3 (Social Network Master Data)](#nível-3-(Social Network Master Data))
		- [Vista Lógica](#vista-lógica-2)
		- [Vista de Processos](#vista-de-processos-2)
               - [SD UC03](#SD-UC03-(N3))
               - [SD US05](#sd-us05-(N3))
               - [SD UC06](#SD-UC06-(N3))
               - [SD UC07](#SD-UC07-(N3))
               - [SD UC07](#SD-UC08-(N3))
               - [SD US09](#sd-us09-(N3))
               - [SD UC10](#SD-UC10-(N3))
               - [SD UC11](#SD-UC11-(N3))
               - [SD UC12](#SD-UC12-(N3))
               - [SD UC33](#SD-UC33-(N3))
               - [SD UC35](#SD-UC35-(N3))
		- [Vista de Implementação](#vista-de-implementação-1)
		- [Vista Física](#vista-física-1)
     - [ Nível 3 (Feed Master Data)](#nível-3-(Feed Master Data))
		- [Vista de Implementação](#vista-de-implementação-2)
		- [Vista Lógica](#vista-lógica-3)
		- [Vista de Processos](#vista-de-processos-3)
               - [SD UC13](#SD-UC13-(N3))

# Views

## Introduction
Será adotada a combinação de dois modelos de representação arquitetural: C4 e 4+1.

O Modelo de Vistas 4+1 [[Krutchen-1995]](References.md#Kruchten-1995) propõe a descrição do sistema através de vistas complementares permitindo assim analisar separadamente os requisitos dos vários stakeholders do software, tais como utilizadores, administradores de sistemas, project managers, arquitetos e programadores. As vistas são deste modo definidas da seguinte forma:

- Vista lógica: relativa aos aspetos do software visando responder aos desafios do negócio;
- Vista de processos: relativa ao fluxo de processos ou interações no sistema;
- Vista de desenvolvimento: relativa à organização do software no seu ambiente de desenvolvimento;
- Vista física: relativa ao mapeamento dos vários componentes do software em hardware, i.e. onde é executado o software;
- Vista de cenários: relativa à associação de processos de negócio com atores capazes de os espoletar.

O Modelo C4 [[Brown-2020]](References.md#Brown-2020)[[C4-2020]](References.md#C4-2020) defende a descrição do software através de quatro níveis de abstração: sistema, contentor, componente e código. Cada nível adota uma granularidade mais fina que o nível que o antecede, dando assim acesso a mais detalhe de uma parte mais pequena do sistema. Estes níveis podem ser equiparáveis a mapas, e.g. a vista de sistema corresponde ao globo, a vista de contentor corresponde ao mapa de cada continente, a vista de componentes ao mapa de cada país e a vista de código ao mapa de estradas e bairros de cada cidade.
Diferentes níveis permitem contar histórias diferentes a audiências distintas.

Os níveis encontram-se definidos da seguinte forma:
- Nível 1: Descrição (enquadramento) do sistema como um todo;
- Nível 2: Descrição de contentores do sistema;
- Nível 3: Descrição de componentes dos contentores;
- Nível 4: Descrição do código ou partes mais pequenas dos componentes (e como tal, não será abordado neste DAS/SAD).

Pode-se dizer que estes dois modelos se expandem ao longo de eixos distintos, sendo que o Modelo C4 apresenta o sistema com diferentes níveis de detalhe e o Modelo de Vista 4+1 apresenta o sistema de diferentes perspetivas. Ao combinar os dois modelos torna-se possível representar o sistema de diversas perspetivas, cada uma com vários níveis de detalhe.

Para modelar/representar visualmente, tanto o que foi implementado como as ideias e alternativas consideradas, recorre-se à Unified Modeling Language (UML) [[UML-2020]](References.md#UML-2020) [[UMLDiagrams-2020]](References.md#UMLDiagrams-2020).

## Nível 1
### Vista De Cenários
![N1-VC](views/n1/N1_VC.svg) 

### Vista Lógica
![N1-VL](views/n1/N1_VL.svg)

### Vista de Processos

#### SSD UC03
![UC5-N1](diagram/level1/UC03_N1_VP.svg)
#### SSD US05
![UC5-N1](diagram/level1/UC5_N1_VP.svg)

#### SSD UC06
![UC5-N1](diagram/level1/UC6_N1_VP.svg)
#### SSD UC07
![UC7-N1](diagram/level1/UC07_N1_VP.svg)

#### SSD UC08
![UC8-N1](diagram/level1/UC8_N1_VP.svg)

#### SSD US09
![UC9-N1](diagram/level1/UC9_N1_VP.svg)

#### SSD UC10
![UC10-N1](diagram/level1/UC10_N1_VP.svg)
#### SSD UC11
![UC11-N1](diagram/level1/UC11_N1_VP.svg)

#### SSD UC12
![UC12-N1](diagram/level1/UC12_N1_VP.svg)

#### SSD UC33
![UC33-N1](diagram/level1/UC33_N1_VP.svg)

#### SSD UC35
![UC33-N1](diagram/level1/UC35_N1_VP.svg)

#### SSD UC13
![UC13-N1](diagram/level1/UC13_N1_VP.svg)

## Nível 2
### Vista Lógica
![N2-VL](views/n2/N2_VL.svg)

### Vista de Processos

#### SSD UC03 (N2)
![UC3-N2](diagram/level2/UC03_N2_VP.svg)

#### SSD US05
![UC5-N2](diagram/level2/UC5_N2_VP.svg)

#### SSD UC06
![UC6-N2](diagram/level2/UC06_N2_VP.svg)

#### SSD UC07
![UC7-N2](diagram/level2/UC07_N2_VP.svg)

#### SSD UC08
![UC8-N2](diagram/level2/UC8_N2_VP.svg)

#### SSD US09
![UC9-N2](diagram/level2/UC9_N2_VP.svg)

#### SSD UC10
![UC10-N2](diagram/level2/UC10_N2_VP.svg)
#### SSD UC11
![UC11-N2](diagram/level2/UC11_N2_VP.svg)

#### SSD UC12
![UC12-N2](diagram/level2/UC12_N2_VP.svg)

#### SSD UC33
![UC33-N2](diagram/level2/UC33_N2_VP.svg)

#### SSD UC35
![UC35-N2](diagram/level2/UC35_N2_VP.svg)

#### SSD UC13
![UC13-N2](diagram/level2/UC13_N2_VP.svg)

### Vista de Implementação
![N2-VI](views/n2/N2_VI.svg)

### Vista Física

Uma proposta muito simplificada. 
![N2-VF](views/n2/N2_VF.svg)

De facto, deve-se ter em consideração os requisitos não funcionais ["Physical Contraints"](Background.md#Physical_Constraints).

### Vista Física + Vista Lógica
![N2-VF+VL](views/n2/N2_VL+VF.svg)

### Vista Física + Vista Implementação
![N2-VF+VI](views/n2/N2_VI+VF.svg)

### Vista Lógica + Vista Implementação
![N2-VL+VI](views/n2/N2_VLxVI.svg)


## Nível 3 (Social Network Master Data)
### Vista Lógica

Baseada numa arquitetura por camadas concêntricas (Onion):
![N3-VL](views/n3/N3_VL.svg)


### Vista Lógica Alternativa
![N3-VL_alt](views/n3/N3_VL_alt.svg)


### Vista de Processos

#### SD UC03
![UC5-N3](diagram/level3/UC03_N3_VP.svg)
#### SSD US05
![UC5-N3](diagram/level3/UC5_N3_VP.svg)

#### SSD UC06
![UC5-N3](diagram/level3/UC06_N3_VP.svg)
#### SSD UC07
![UC7-N3](diagram/level3/UC07_N3_VP.svg)

#### SSD UC08
![UC8-N3](diagram/level3/UC8_N3_VP.svg)

#### SSD US09
![UC9-N3](diagram/level3/UC9_N3_VP.svg)

#### SSD UC10
![UC11-N3](diagram/level3/UC10_N3_VP.svg)

#### SSD UC11
![UC11-N3](diagram/level3/UC11_N3_VP.svg)

#### SSD UC12
![UC12-N3](diagram/level3/UC12_N3_VP.svg)

#### SSD UC33
![UC33-N3](diagram/level3/UC33_N3_VP.svg)

#### SSD UC35
![UC12-N3](diagram/level3/UC35_N3_VP.svg)

### Vista de Implementação
![N3-VI](views/n3/N3_VI.svg)

### Vista de Implementação Alternativa
![N3-VI_alt](views/n3/N3_VI_alt.svg)

### Vista Lógica + Vista Implementação
![N3-VL+VI](views/n3/N3_VLxVI.svg)


### Vista Física

Por agora, não existe necessidade de ser representada.

## Nível 3 (SPA)

### Vista Implementação
![N3-VL](views/n3/N3_VI_SPA.svg)

### Vista Lógica
![N3-VL](views/n3/N3_VL_SPA.svg)

## Nível 3 (Feed Master Data)

### Vista Implementação
![N3-VL](views/n3/N3_VI_SPA.svg)

### Vista Lógica
![N3-VL](views/n3/N3_VL_SPA.svg)

### Vista de Processos

#### SD UC13
![U13-N3](diagram/level3/UC13_N3_VP.svg)