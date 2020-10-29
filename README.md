# Метрические алгоритмы классификации

Одной из областей применения машинного обучения является задача классификации:
есть некоторое, зараенее известное, множество классов и нам требуется отнести к
одному из них классифицируемый объект.

В метрических алгоритмах классификации суждения о принадлежности объекта к
классу основываются на некоторой функции метрики и на гипотезе компактности.
Гипотеза компактности говорит о том, что на схожих объектах алгоритм дает
одинаковые ответы или, другими словами, если объекты находятся в пространстве
рядом, то они относятся к одному классу.

Сравнение осуществленных алгоритмов:
|Алгоритм       |параметры           |ошибка|
|---------------|--------------------|------|
|1NN            |k = 1               |7     |
|kNN            |k = 6               |5     |
|**kwNN**       |**k = 3, q = 0.7**  |**6** |
|Parzen         |h = 0.4             |6     |
|Potential      |h = 0.4,            |6     |

<table>
 <tr>
  <td>
   <img src="kNN/kNN_CM.png" width="500" heigth="200">
  </td>
  <td>
   <img src="kwNN/kwNN_CM.png" width="500" heigth="200">
  </td>
</tr>

 <tr>
  <td>
   <img src="parzen/parzen_CM_epan.png" width="500" heigth="200">
  </td>
  <td>
   <img src="poten/poten_CM_full.png" width="500" heigth="200">
  </td>
 </tr>
</table>

## Лабораторная №1

### kNN/oneNN.R
*Метод 1-го ближайщего соседа.*

Алгоритм 1nn - частный случай алгоритма knn. Он относит классифицируемый объект
к тому классу, элемент которого находится ближе всего к классифицируемому.

Однако такой алгоритм не очень устойчив к выбросам. Поэтому чаще пользуются
алгоритмом knn c настраиваемым параметром k.

Файл содержит скрипт на языке R, реализующий алгоритм классификации 1nn.
Данный алгоритм принимает на вход обучающую выбрку, представленную в виде
набора векторов-признаков и меток класса, и классфифицируемый объект,
а отдает метку класса объекта, который лежит к классифицируемому ближе
остальных (в соответствии с выбранной метрикой).

Функция ***EM*** - функция метрики, принимает на вход два вектора, отдает на
выход расстояние между векторами. Выбрана евклидова метрика.

Функция ***ruler*** измеряет расстояния от классифицируемого объекта до
элементов обучающей выборки. Принимает на вход классифицируемый объект, матрицу
векторов обучающей выборки и функцию метрики, отдает отсортированный по
возрастанию массив расстояний.

Функция ***oneNN*** реализует алгоритм 1nn. Принимает на вход классифицируемый
объект и матрицу признаков, отдает предполагаемый класс.

Далее приводится пример работы программы с отрисовкой графика: классифицируется
10 объектов,
обучающая выборка - ирисы Фишера.

![](kNN/oneNN__plot.png)

### kNN/kNN.R
*Метод k ближайших соседей.*

Алгоритм knn - алгоритм классификации, который относит классифицируемый объект
к тому классу, элементов которого больше среди первых k ближайших элементов
обучающей выборки.

Однако изначально непонятно при каком k функционал эмпирического риска будет
минимален. Для того, чтобы это выяснить пользуются критерием скользящего
контроля: обучающая выборка определенное количество раз, различным образом
делится на две непересекающиеся: на новую обучающую (training set) и на
проверяющую (test set), и модель обучается на неполных данных, а потом
оценивается количество ошибок на проверяющей выборке.
Мы используем алгоритм скользящего контроля LOO, в котором на каждом шаге в
проверяющую выборку входит лишь один объект и алгоритм осуществляет лишь l 
переобучений (где l - величина обучающей выборки).

Файл содержит скрипт на языке R, реализующий алгоритм классификации knn.
В методе k ближайших соседей на вход алгоритму подается обучающая выборка и
классифицируемый объект, на выходе - класс объекта, совпадающий с классом
большинства из k ближайших соседей.

Функции ***EM*** и ***ruler*** аналогичны соответствующим из *oneNN.R*.

Функция ***kNN*** реализует алгоритм 1nn. Принимает на вход обучающую матрицу
признаков, массив меток, классифицируемый объект и количество k соседей.
Возвращает предполагаемый класс.

Функция ***kNN_LOO*** принимает на вход матрицу признаков, метки, минимальное и
максимальное значение подбираемого параметра, флаг отрисовки графика. На выходе:
оптимальное значение параметра.

График зависимости ошибки от k:
![](kNN/kNN__loo_plot.png)

Пример работы программы - классификация 10 рандомных точек:
![](kNN/kNN__plot.png)

Карта классификации ирисов Фишера алгоритмом kNN, при k = 6:
![](kNN/kNN_CM.png)

### kwNN/kwNN.R
*Метод k взвешенных ближайших соседей.*

В алгоритме knn голоса всех k соседей равны. В некоторых задачах это приводит
к большему числу ошибок. С этим помогает справиться алгоритм kwnn, в котором
каждому соседу, в зависимости от его ранга, присваивается значение некоторой
невозрастающей функции w(i), где i - это ранг соседа. Получается, что в этом
алгоритме добавляется еще один настраиваемый параметр - вес голоса соседа,
оптимальное значение которого подбирается с помощью LOO.
 
Файл содержит скрипт на языке R, реализующий алгоритм классификации kwnn.
На вход алгоритму подается обучающая выборка, классифицируемый объект число
соседей k и весовая функция w(i), а возвращается класс классифицируемого
объекта.

Функции ***EM*** и ***ruler*** аналогичны соответствующим из *oneNN.R*.

Функция ***kwNN*** реализует алгоритм kwnn. Принимает на вход обучающую матрицу
признаков, массив меток, классифицируемый объект, количество k соседей и
знаменатель q геометрической прогрессии (в виде весовой функции выбрана
геометрическая прогрессия со знаменателем из интервала (0, 1). Возвращает
предполагаемый класс.

Функция ***kwNN_LOO*** аналогична функции ***kNN_LOO*** из файла *kNN.R*.

Тепловая карта зависимости ошибки от k и q:
![](kwNN/kwNN_heatMap.png)

Пример работы программы - классификация 10 рандомных точек:
![](kwNN/kwNN__plot.png)

Карта классификации ирисов Фишера алгоритмом kwNN, при k = 3, q = 0.6:
![](kwNN/kwNN_CM.png)

Пример, показывающий преимущество метода kwNN над kNN:
- kNN-классификация объекта:
![](compare/compare_kNN.png)

- kwNN-классификация объекта:
![](compare/compare_kwNN.png)

## Лабораторная №2

### parzen/parzen.R
*Метод парзеновского окна.*

В алгоритме k взвешенных ближайших соседей вес голосу соседа присваивается
в зависимости от ранга соседа. Этот подход не всегда приводит к желаемым
результатам. Вместо него можно использовать *метод парзеновского окна*, в котором
вес голосу соседа присваивается в зависимости от расстояния, на котором
находится сосед, а соседями считаются все объекты обучающей выборки, которые
находятся в некоторой h-окрестности классифицируемого объекта.

Файл содержит скрипт на языке R, реализующий алгоритм классификации с помощью
метода парзеновского окна.

Функции ***EM*** и ***ruler*** аналогичны соответствующим из *oneNN.R*.

Функции ***epan***, ***quar***, ***tria***, ***gaus***, ***rect*** - ядра
Епанечникова, квартическое, треугольное, гауссовское, прямоугольное
соответственно. Вход: вещественное число, выход: вещественное число - значение
ядра в этой точке.

Функция ***parzen*** реаизует метод парзеновского окна. Вход: матрица признаков,
вектор меток, классифицируемый объект, ширина окна h, функция ядра.
Выход: класс.

Функция ***parzen_LOO*** подбирает оптимальный параметр h ширины окна.
Вход: матрица признаков, вектор меток, минимальное и максимальное значение
параметра, ядро и флаг отрисовки графика. Выход: оптимальное значение h.

Далее приведены графики loo-функций для разных ядер:
- ядро Епанечникова:
![](parzen/parzen_loo_epanechnikov.png)
- гауссовское ядро:
![](parzen/parzen_loo_gaus.png)
- квартическое ядро:
![](parzen/parzen_loo_quar.png)
- прямоугольное ядро:
![](parzen/parzen_loo_rect.png)
- треугольное ядро:
![](parzen/parzen_loo_tria.png)

Карты классификации для разных ядер:
- ядро Епанечникова:
![](parzen/parzen_CM_epan.png)
- гауссовское ядро:
![](parzen/parzen_CM_gaus.png)
- квартическое ядро:
![](parzen/parzen_CM_quar.png)
- прямоугольное ядро:
![](parzen/parzen_CM_rect.png)
- треугольное ядро:
![](parzen/parzen_CM_tria.png)

## Лабораторная №3

### poten.R
*Метод потенциальных функций.*

Если в методе парзеновского окна центр окна помещать не в классифицируемый
объект, а в обучающий, при чем у каждого обучающего объекта своя ширина окна
и каждый обучающий объект обладает некоторым потенциалом - неотрицательным
числом, на которое домножаются ядра, то получим метод потенциальных функций.

Файл содержит скрипт на языке R, реализующий алгоритм классификации с помощью
метода потенциальных функций.

Функции ***EM*** и ***ruler*** аналогичны соответствующим из *oneNN.R*.

Функции ***epan***, ***quar***, ***tria***, ***gaus***, ***rect*** аналогичны
соответствующим из *parzen.R*.

Функция ***poten*** реализует метод потенциалов. Вход: матрица признаков,
вектор меток, классифицируемый объект, функция ядра, вектор ширин окон h и
вектор потенциалов gamma. Выход: класс.

Функция ***gamma_set*** подбирает значения потенциалов gamma. 
Вход: матрица признаков, вектор меток и допустимое значние ошибок.
Выход: вектор потенциалов.

Карта выборки с учетом потенциалов объектов:
![](poten/poten_raspr.png)

Карта классификации:
- с учетом потенциалов:
![](poten/poten_CM.png)
- без:
![](poten/poten_CM_full.png)

## Лабораторная №4

### margin.R
*Отступы*

Часто возникает такая проблема: обучающая выборка сильно большая, что приводит
к увеличению времени обучения модели. Чтобы решить эту проблему пользуются
алгоритмами, связанными с понятием отступа. Отступы показывают степень
типичности объекта для его класса относительно какого-либо алгоритма и
считаются так: отступ = (оценка принадлежности объекта своему классу) -
(максимум из оценок принадлежности другим классам).

Файл содержит скрипт на языке R, отрисовывающий график зависимости отступа от
элемента выборки ирисов Фишера относительно алгоритма парзеновского окна, с
шириной окна - 0.4.

Функции ***EM*** и ***ruler*** аналогичны соответствующим из *oneNN.R*.

Функции ***epan***, ***parzen*** аналогичны соответствующим из *parzen.R*.

Функция ***margin*** считает отсутп элемента относительно обучающей выборки
и алгоритма парзеновского окна. Вход: матрица признаков, вектор классов,
элемент, для которого считается отступ, номер этого элемента в списке
ирисов Фишера. Выход: посчитанный отступ.

Функция ***margin_plot*** считает отступы для всех элементов обучающей выборки,
сортирует их по возрастанию и отрисовывает. Вход: матрица признаков и вектор
классов.

График отступов ирисов Фишера:
![](margin__plot_parzen.png)

*Алгоритм крутого склона*

Алгоритм крутого склона позволяет убрать из выборки шумы (элементы с большим
отрицательным отступом). Все отступы сортируются в порядке их возрастания,
затем находится такой элемент с отрицаетльным отступом, у которого разность
отступов с предыдущим максимальна среди всех соседних элементов с
отрицатлеьными отступами, после чего все элементы до найденного убираются из
обучающей выборки.

Функция ***cool_descent*** реализует алгоритм крутого склона. Вход: массив
отступов. Выход: массив отступов без шумов.

### STOLP.R

*Алгоритм STOLP*

Одним из алгоритмов, решающих проблему минимизации обучающих данных, является
STOLP-алгоритм. Он, изначально посчитав отступы обучающей выборки, выбирает
из каждого класса по одному элементу с наибольшим отступом, и добавляет в
множество Ω, далее он смотрит отступы элементов обучающей выборки без элементов
множества Ω, и
- если количество элементов с отрицательным отступом не превышает
некоторой, заранее заданной, допустимой величины, то он преращает выполнение
алгоритма, а множество Ω - множество эталонов,
- иначе он добавляет в Ω элемент с наименьшим отступом и заново проверят
отступы оставшихся элементов обучающей выборки.

Функции ***EM*** и ***ruler*** аналогичны соответствующим из *oneNN.R*.

Функции ***epan***, ***parzen*** аналогичны соответствующим из *parzen.R*.

Функция ***margin*** считает отступы элементов некоторого множества
относительно другого множества и некоторого алгоритма. Вход: матрица признаков,
вектор классов, матрица признаков и вектор классов объектов, для которых
считаются отступы. Выход: матрица, в столбцах который: значения признаков,
класс, отступ объектов, для которых считали отступ.

Функция ***err*** считает отступы объектов обучающей выборки.
Вход: матрица признаков, вектор классов. Выход аналогичен выходу функции
***margin***.

Функция ***STOLP*** реализует алгоритм STOLP. Вход: матрица признаков, вектор
классов, значение допустимой ошибки. Выход: множество эталонов.

Множество эталонов ирисов Фишера для алгоритма парзеновского окна с
ядром Епанечникова, шириной окнв - 0.4:
![](STOLP/STOLP__etalon.png)

# Байесовские алгоритмы классификации

## Лабораторная №5

### level_curves.R

*Линии уровня*

Одним из способов представления функций многих переменных являются линии
уровня: на разных значениях n-мерной функции проводятся гипперплоскости и все
точки пересечения этой гипперплоскости с функцией отображаются на (n-1)-мерное
пространство. Для функции двух переменных эти точки пересечения отображаются
на плоскость XOY.

Файл содержит скрипт на языке R, отрисовывающий линии уровня для заданного
n-мерного нормального распределения (которое задается матожиданием и матрицей
ковариации).

Функция ***density*** считает значение плотности распределения с определенными
матожиданием и матрицей ковариации в точке. Вход: координаты точки, матождание
и матрица ковариации. Выход: значение плотности в точке.

Функция ***plotin*** отрисовывает линии уровня плотности распределения.
Вход: матожидание и матрица ковариации.

Графики линий уровня нормального распределения с разными матрицами ковариации
и одинаковым матожиданием (М = (3, 3)):
- некорелированные признаки, одинаковые дисперсии:
![](level_nekor_sameDisp.png)

- некорелированные признаки, разные дисперсии:
<table>
	<tr>
		<td>
			<img src="level_nekor_nsameDisp1.png" width="500" heigth="200">
		</td>
		<td>
			<img src="level_nekor_nsameDisp2.png" width="500" heigth="200">
		</td>
	</tr>
</table>

- корелированные признаки, разные дисперсии:
![](level_kor_nsameDisp.png)
