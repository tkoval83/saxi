# Контролер AxiDraw на Scala

## Огляд

Scala-контролер для машини малювання AxiDraw з підтримкою рендерингу тексту за допомогою шрифтів Hershey.

## Можливості

- Рендеринг тексту з використанням масштабованих шрифтів Hershey.

## Встановлення

1. **Передумови**:
    - Встановлений Scala (стабільна версія)
    - Пристрій AxiDraw, підключений через USB

2. **Клонування репозиторію**:
   ```bash
   git clone https://github.com/tkoval83/saxi.git
   cd saxi
   ```

3. **Збірка проекту**:
   ```bash
   sbt clean assembly
   ```

4. **Запуск**:
   Після успішної збірки, ви можете запустити програму за допомогою:
   ```bash
   java -jar target/scala-2.13/saxi-assebly-1.0.jar
   ```
