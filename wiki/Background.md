## Contents
- [Architecture Background](#architecture-background)
	- [Problem Background](#problem-background)
		- [System Overview](#system-overview)
		- [Context](#context)
		- [Driving Requirements](#driving-requirements)
			- [Functional requirements](#functional-requirements)
			- [Quality attributes](#quality-attributes)
				- [Funcionalidade](#funcionalidade)
				- [Usabilidade](#usabilidade)
				- [Confiabilidade (Reliability)](#confiabilidade-reliability)
				- [Desempenho (Performance)](#desempenho-performance)
				- [Suportabilidade](#suportabilidade)
				- [Design constraints](#design-constraints)
				- [Implementation constraints](#implementation-constraints)
				- [Interface constraints](#interface-constraints)
				- [Physical constraints](#physical-constraints)
	- [Solution Background](#solution-background)
		- [Architectural Approaches](#architectural-approaches)
		- [Analysis Results](#analysis-results)
		- [Mapping Requirements to Architecture](#mapping-requirements-to-architecture)

# Architecture Background
>Architecture Background provides information about the software architecture, by:
>- describing the background and rationale for the software architecture;
>- explaining the constraints and influences that led to the current architecture;
>- describing the major architectural approaches that have been utilized in the architecture.
  
## Problem Background
>The sub-parts of this section explain the constraints that provided the significant influence over the architecture.

### System Overview
> This section describes the general function and purpose for the system or subsystem whose architecture is described in this SAD.

A Graph4Social pretende uma simulação de um jogo que é uma rede social, onde o jogador tem como objetivo criar a mais forte e maior rede social possível. Através da execução de missões, o jogador consegue aumentar a sua rede na leader board. 


### Context
> This section describes the goals and major contextual factors for the software architecture. The section includes a description of the role software architecture plays in the life cycle, the relationship to system engineering results and artifacts, and any other relevant factors.

Pretende-se o desenvolvimento de um protótipo para um jogo baseado na visualização e manipulação de grafos sociais. O jogo desenrola-se numa série de missões em que o jogador terá que avançar e aumentar a sua rede. 

*NB: Contudo, o sistema aqui pedido é uma simplificação daquilo que é um sistema de gestão de transportes, pelo que são assumidas simplificações para tornar o projeto exequível neste âmbito (i.e. 5º semestre da LEI).*

Nesta edição do 5º semestre da LEI teremos a colaboração da empresa Graphs4Social, S.A com sede no Porto (Portugal) que tem a  missão de fornecer aplicações com o objetivo de  manipular e visualizar grafos de redes sociais. A empresa decidiu recentemente expandir o seu portfolio de produtos entrando na área de jogos, mantendo o foco nos grafos de redes sociais.

Este SAD serve de base para debate sobre o sistema a construir (a implementar, testar e implantar), e pretende-se que esteja alinhado com o sistema construído. Para além do óbvio na descrição duma arquitetura de software, deve identificar alternativas de design e ponto de variação.

### Driving Requirements
> This section lists the functional requirements, quality attributes and design constraints. It may point to a separate requirements document.

#### Functional requirements
3. Como jogador pretendo editar o meu relacionamento com tags e força de ligação.
5. Como jogador pretendo editar o meu perfil próprio.
6. Como jogador pretendo editar o meu estado de humor.
7. Como jogador pretendo consultar a minha rede a partir da minha perspetiva.
8. Como utilizador não autenticado pretendo registar-me no sistema.
9. Como jogador recém registado pretendo escolher quais "utilizadores objetivo" (sugeridos pelo sistema) gostaria de ter na minha rede.
10. Como jogador quero pesquisar utilizadores que conheça na minha rede, e pedir ligação de utilizador.
11. Como jogador pretendo pedir introdução a um utilizador objetivo.
12. Como jogador pretendo aprovar/desaprovar pedidos de introdução.
33. Como jogador pretendo aceitar ou rejeitar intordução.
35. Como jogador pretendo obter lista de pedidos de ligação pendentes.

![N1-VC](views/n1/VC/N1_VC.svg)

#### Quality attributes
Os atributos de qualidade são categorizados e sistematizados segundo o modelo [FURPS+](https://pt.wikipedia.org/wiki/FURPS).

##### Funcionalidade
1. Cada sistema só poderá aceder aos dados que lhe dizem respeito.

2. Deve ser auditada e verificada a integridade da informação a que os sistemas acedem.

3. Com vista à necessidade de saber e necessidade de conhecer, toda a informação deve estar protegida de acessos indevidos. Ou seja, o princípio de minimização de acesso ao que é essencial para cada utilizador/aplicação, criação de túneis para transferência de informação, avaliação da integridade de dados e aplicações, e encriptação/minimização dos dados.

4. Uma vez que o módulo de gestão de encomendas se encontra virado para o exterior, é necessário ter especial atenção com a privacidade e proteção de dados à luz do RGPD. Assim é necessário que o sistema cumpra a legislação em vigor e, em especial, disponibilize as informações legais e informe o utilizador aquando do seu registo, bem como permita aceder e cancelar a sua conta nos casos e nas condições legalmente permitidas.

5. Todas as aplicações devem ter em consideração as perspetivas de internacionalização da empresa e por isso devem suportar localização do software para, pelo menos, Português e Inglês. 


##### Usabilidade
6. O Módulo de navegação e visualização 3D deverá permitir a visualização em 3D do grafo da rede social do utilizador. Deverá também apresentar, em sobreposição, um mini-mapa 2D de todo o grafo da rede social. O módulo deverá suportar áudio para os efeitos especiais (por exemplo, chuva, trovões) e eventual música de fundo.

7. O módulo visualizador deve mostrar de forma visual diferenciada o caminho de ligação entre o jogador/utilizador que está autenticado e um outro utilizador (escolhido com o rato ou através de uma pesquisa).

##### Confiabilidade (Reliability)
n/a

##### Desempenho (Performance)
n/a

##### Suportabilidade
8. As máquinas de instalação e demonstração do sistema devem ser máquinas de produção e não máquinas de desenvolvimento; isto é, não devem ter instaladas as ferramentas de desenvolvimento nem devem executar as aplicações dentro dos ambientes de desenvolvimento.

##### Design constraints
9. Todas as aplicações devem possuir uma organização em camadas separando os componentes de apresentação (interface humano-computador) dos componentes de processamento e acesso a dados recorrendo às boas práticas da indústria.

![Visão geral do sistema definido no enunciado/caderno de encargos](diagramas/visaogeralsistema_enunciado.png)


<img src="diagramas/visaogeralsistema_enunciado.png" width="75%">

De um modo geral, as principais funcionalidades de cada módulo são as seguintes:

- Master data – permite a gestão da informação relacionada com a rede (nós, percursos), tipos de viaturas, tipos de tripulantes, linhas e viagens.
- Planeamento – com base nos percursos existentes planear as trocas de tripulações nos pontos de rendição. Planear os serviços de tripulantes com base nos serviços de viatura. Consome a informação gerida no módulo master data e publica informação do planeamento para o módulo de visualização.
- Visualizador 3D –  permite a visualização 2D e 3D da rede, a navegação pela cena e a consulta gráfica de informação sobre as viagens. Consome a informação gerida no módulo master data e no módulo
- UI – interface com o utilizador
- Clientes + RGPD – gestão de informação dos utilizadores finais “clientes” e seus consentimentos no âmbito do RGPD

9.  No âmbito do projeto atual, a administração de utilizadores pode ser efetuada diretamente na base de dados não sendo necessário um módulo de gestão de utilizadores.

10.  Embora não esteja no âmbito atual do projeto, deve ser levado em conta na arquitetura da solução, a extensão futura para aplicações móveis.

##### Implementation constraints
11.   Todos os módulos devem fazer parte do código fonte da mesma SPA e serem disponibilizados como um único artefacto.

##### Interface constraints
12.  A SPA deve permitir acesso a todos os módulos do sistema: master data, planeamento e visualização, bem como RGPD. (repetida porque diz respeito a duas categrorias)
13.  O módulo de Planeamento deve consumir dados de rede através da API do master data
14.  O módulo de Planeamento deve consumir dados de viagens através da API do master data
15.  O módulo de Visualização deve consumir dados de rede através da API do master data
16.  O módulo de Visualização deve consumir dados de viagens através da API do master data "viagens"
17.  O módulo de Visualização deve consumir dados de serviços de tripulante através da API do planeamento


##### Physical constraints
18. Existem dois servidores em load balancing, onde estão instaladas as aplicações, serviços e as bases de dados e que se encarregam do armazenamento da informação.

19. Existem ainda dois servidores em failover que distribuem os endereços a todos os sistemas e se encarregam da autenticação de sistemas e utilizadores (DHCP, DNS (se aplicável) e autenticação de servidores, e eventualmente um servidor Kerberos).
20. Algumas das aplicações devem ser implantadas *on premises* e outras em IaaS e PaaS (*on cloud*). Cf. requisitos específicos das UC por sprint.

## Solution Background
> The sub-parts of this section provide a description of why the architecture is the way that it is, and a convincing argument that the architecture is the right one to satisfy the behavioral and quality attribute goals levied upon it.

### Architectural Approaches
> This section provides a rationale for the major design decisions embodied by the software architecture. It describes any design approaches applied to the software architecture, including the use of architectural styles or design patterns, when the scope of those approaches transcends any single architectural view. The section also provides a rationale for the selection of those approaches. It also describes any significant alternatives that were seriously considered and why they were ultimately rejected. The section describes any relevant COTS issues, including any associated trade studies.

Baseado nos requisitos não funcionais e restrições de design, serão adotadas as seguintes abordagens/padrões/estilos:

- Client-Server, porque cada um dos "módulos" Graphic Interface, Artificial Intelligence e Social Network Master Data são aplicações servidoras de outras aplicações clientes (e.g. Social Network Master Data é servidor de Artificial Intelligence e Graphic Interface, Artificial Intelligence é servidor de Graphic Interface);
- Web Application, em que o frontend é desempenhado por uma SPA (Single Page Application), e que o backend é desempenhado pelos módulos Artificial Intelligence e Social Network Master Data;
- SOA, porque os servidores (cf. anterior) deverão disponibilizar API, e particularmemte API para serem usadas na web, disponibilizados serviços para os clientes respetivos. Serão adotados os nível 0, 1 e 2 do [Modelo de Maturidade de Richardson](https://martinfowler.com/articles/richardsonMaturityModel.html) aplicado a REST;
- N-Tier, pois as várias aplicações devem ser implantadas em diferentes máquinas *on premises* e IaaS e PaaS (*on cloud*), de acordo com os requisitos não funcionais;
- Layered architecture, mais especificamente Onion Architecture, por razões académicas.

Outras abordagens/estilos/padrões, como e.g. interligação entre aplicações baseado em mensagens-eventos foram desconsideradas para não violar os requisitos e restrições definidos, mas também por questões académicas.

### Analysis Results
> This section describes the results of any quantitative or qualitative analyses that have been performed that provide evidence that the software architecture is fit for purpose. If an Architecture Tradeoff Analysis Method evaluation has been performed, it is included in the analysis sections of its final report. This section refers to the results of any other relevant trade studies, quantitative modeling, or other analysis results.

Não existem por agora resultados de análise ou avaliação. Estudos qualitativos acerca dos estilos/padrões adotados (nomeadamente Onion em Social Network Master Data, mas também Dependency Injection na UI), permitem empiricamente advogar que a manutenibilidade, evolutabilidade e testabilidade do software são elevadas, ao mesmo tempo que permitem atingir as funcionalidades desejadas.

### Mapping Requirements to Architecture
> This section describes the requirements (original or derived) addressed by the software architecture, with a short statement about where in the architecture each requirement is addressed.

TBD