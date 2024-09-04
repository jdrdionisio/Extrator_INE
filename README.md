<a name="top"></a>
<h3 align="center">"Aplicação "</h3>

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Índice</summary>
  <ol>
    <li>
      <a href="#Sobre">Sobre o projeto</a>
      <ul>
	<li>
      <a href="#Sobre">Autores</a>
      <ul>
        <li><a href="#Estrutura-das-Pastas">Estrutura das Pastas</a>
          <ul>
            <li><a href="#Documentation">Documentation</a></li>
            <li><a href="#Datasets">Datasets</a></li>
            <li><a href="#Mapas">Mapas</a>
              <ul>
                <li><a href="#Shapefiles">Shapefiles</a></li>
                <li><a href="#Imagens">Imagens</a></li>
				  </ul>
				</li>
			  </ul>
			</li>
		  </ul>
		</li>
      </ul>
    </li>
    <li>
      <a href="#Validação">Validação</a>
      <ul>
        <li><a href="#Pré-requisitos">Pré-requisitos</a></li>
      </ul>
    </li>
    <li><a href="#Contacto">Contacto</a></li>
  </ol>
</details>

<!-- ABOUT THE PROJECT -->
## Sobre

O projeto "Extrator INE" foi desenvolvido com o objetivo de facilitar a extração e manipulação de dados estatísticos do Instituto Nacional de Estatística (INE), que disponibiliza uma vasta gama de dados socioeconómicos através de sua API com utilidade para a Saúde Pública Nacional. 
Este extrator foi projetado para consumir os serviços da API do INE, extrair dados formatados em JSON, e convertê-los para csv que podem ser facilmente utilizados para análise e geração de relatórios.

O programa utiliza funções que podem ser adaptadas para uso em relatórios automáticos:
ine.get - Extração de dados
	Por base está feita para minimizar erros do utilizador
		Extrai tudo se possível e filtra o necessário (1 call para a API)
		A extração não usa anoa mas observações para atrás minizando pedidos com dados que não existem
		Testa apriori todas as subdivisões administrativas para escolher a que se aproxima do que foi pedido pelo utilizador
ine.meta - Extração de metadados
	Faz manipulação dos dados recolhidos e exporta como csv
	
API de referência, disponível em ([Link](https://www.ine.pt/xportal/xmain?xpid=INE&xpgid=ine_api&INST=322751522&ine_smenu.boui=357197120&ine_smenu.selected=357197822&xlang=pt)).

Os logos das seguintes instituições foram utilizados no projeto com os seguintes propósitos:

    Direção-Geral da Saúde (DGS) e Serviço Nacional de Saúde (SNS): Representam a afiliação profissional no momento da execução do projeto, indicando a vinculação institucional dos colaboradores à DGS e ao SNS.
    Instituto Nacional de Estatística (INE): Utilizado para indicar a fonte dos dados extraídos e processados durante o projeto.

É importante destacar que o uso desses logos não implica nenhum tipo de patrocínio, apoio formal ou aval institucional por parte da DGS, DE-SNS ou INE em relação ao projeto ou ao código resultante. 

O projeto foi conduzido de maneira independente, e o resultado final não representa de forma oficial as posições, visões ou diretrizes dessas instituições. 

O uso dos logos visa apenas cumprir uma função informativa e de atribuição às respectivas fontes e afiliações no contexto do desenvolvimento do projeto.

<p align="right">(<a href="#top">Voltar ao topo</a>)</p>

## Autores

- [@jdrdionisio](https://github.com/jdrdionisio)
- [@rafaelcsv](https://github.com/rafaelcsv)


<p align="right">(<a href="#top">Voltar ao topo</a>)</p>


## Estrutura da pastas:

Por forma a facilitar a consulta do projeto, o mesmo apresenta a seguinte estrutura:

```plaintext
Extrator_INE 
│
├── www           			# Pasta para imagens institucionais
│   ├── DGS.png
│   ├── INE.gif
│   └── SNS.png 
│
├── datasets             	# Pasta para os dados utilizados no projeto, subdividida em:
│   ├── Indicatores.xlsx    	 # - Dados dos indicadores disponíveis pelo INE, retirado de 
│   │ 								[Link](https://smi.ine.pt/Indicador?clear=True)
│   ├── indicadores.csv   		 # - Dados dos indicadores disponíveis pelo INE ficheiro em teste 
│   │ 								para adicionar funcionalidade
│   └── geo_linkage_2024_v2.csv  # - Dados dos indicadores disponíveis pelo INE ficheiro em teste 
│
├── INE.Rproj				# Utilizado para gerenciar os scripts e pacotes necessários ao desenvolvimento
│
├── app.R					# Contém o código da aplicação principal em shiny
│
└── README.md         		# Descrever organização e metodologia
```

Os ficheiros do projeto estão divididos em 2 pastas:

<p align="right">(<a href="#top">Voltar ao topo</a>)</p>

## WWW

Esta pasta contém os logos das instituições associadas ao projeto:

Ficheiros: **DGS.png** , **INE.gif** , **DGS.png**

    DGS.png e SNS.png: Representam a afiliação profissional no momento da execução do projeto, indicando a vinculação institucional dos colaboradores à DGS e ao SNS.
	
    INE.gif: Representa o Instituto Nacional de Estatística, de onde os dados utilizados são retirados.
	
    Nota: O uso destes logos não implica qualquer tipo de patrocínio ou apoio formal por parte das instituições mencionadas. Eles são utilizados exclusivamente para fins informativos, indicando afiliações e fontes de dados.

<p align="right">(<a href="#top">Voltar ao topo</a>)</p>

## Datasets

Existem 2 datasets disponíveis:

Nome: **geo_linkage_2024_v2.csv**

Descrição - Ficheiro de ligação da Freguesia aos outros níveis administrativos e de saúde

Fonte: A atribuição das ULS foi baseada no Decreto-Lei n.º 102/2023 de 7 de novembro de 2023, disponível em **([Link](https://diariodarepublica.pt/dr/detalhe/decreto-lei/102-2023-223906278)).**

Observações: 

A freguesia de Campo de Ourique encontra-se duplicada, uma vez que se encontra dividida entre 2 ULS, para permitir filtros para ambas as ULS sem perda de informação e sem necessidade de ULS com nomes concatenados, foi atribuída a designação múltipla. 

Este ficheiro contém observações pois tem limitações na sua utilização para as ULS, uma vez que, as divisões por concelho podem não ter resolução para poderem ser atríbuidas a uma única ULS, particularmete as relativas às Áreas Metropolitanas. 
Casos são concelho de Lisboa, Loures, Porto e Gondomar.

Nome: **Indicatores.xlsx** e **indicadores.csv**

Descrição: Ficheiro contendo a seleção de indicadores disponíveis no INE, para minimizar erros de introdução de códigos não utilizados.

Fonte: **([Link](https://smi.ine.pt/Indicador?clear=True)).**

Observações: O indicador foi escolhido por ser o que contém maior resolução ao nível da freguesia e idade. Poderá ser extraído outros indicadores do INE utilizando o código do script ine.qmd. Terá de ser mudado as seguintes linhas de código.

1. A freguesia de Campo de Ourique encontra-se a contar para a primeira ULS que faz parte, uma vez que se encontra dividida entre 2 ULS, para permitir população total correcta. Exemplo de execução: (**left_join(geo_chosen, multiple="first")**).

<p align="right">(<a href="#top">Voltar ao topo</a>)</p>

## Screenshots

![App Screenshot](https://ibb.co/GVmPGjj)


<p align="right">(<a href="#top">back to top</a>)</p>

<!-- VALIDATION -->
## Validação

### Deploy or Run

Para fazer o deploy desse projeto pode ser feito no shinyapps.io e pode-se correr com o VScode com o seguinte comando.

```
  shiny::runApp()
```

### Contribuindo

Contribuições são sempre bem-vindas!

### Pré-requisitos

Para correr o projeto é necessário ter instalado o R com uma IDE que permite compilar ficheiros R (por exemplo: Rstudio ou Positron ou VScode).

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- CONTACT -->
## 🔗 Contactos
[![linkedin](https://img.shields.io/badge/linkedin-0A66C2?style=for-the-badge&logo=linkedin&logoColor=white)](https://www.linkedin.com/in/joao-david-dionisio-201875171/)

## Licença

[GNU GPLv3](https://choosealicense.com/licenses/gpl-3.0/)

<p align="right">(<a href="#top">back to top</a>)</p>
