oneNN.R
-------

Файл содержит скрипт на языке R, реализующий алгоритм классификации 1nn.
Данный алгоритм принимает на вход обучающую выбрку, представленную в виде
набора векторов-признаков и меток класса, и классфифицируемый объект,
а отдает метку класса объекта, который лежит к классифицируемому ближе
остальных (в соответствии с выбранной метрикой).

Функция ****EM**** - функция метрики, принимает на вход два вектора, отдает на выход
расстояние между векторами. Выбрана евклидова метрика.

Функция *ruler* измеряет расстояния от классифицируемого объекта до элементов
обучающей выборки. Принимает на вход классифицируемый объект, матрицу векторов
обучающей выборки и функцию метрики, отдает отсортированный по возрастанию
массив расстояний.

Функция *oneNN* реализует алгоритм 1nn. Принимает на вход классифицируемый объект
и матрицу признаков, отдает предполагаемый класс.

Далее приводится пример работы программы с отрисовкой графика: классифицируется 10 объектов,
обучающая выборка - ирисы Фишера.
