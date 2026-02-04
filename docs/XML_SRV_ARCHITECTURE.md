# XML_SRV_ARCHITECTURE

> Основано на общей архитектуре KSeF Integration fileciteturn6file0 и дополнено предложениями по рефакторингу текущего `zcl_ksef_data_helper` (генерация структуры/данных для XML, сравнения для коррекций, извлечение тегов из XML).

## 1. Контекст и проблема

Сейчас `zcl_ksef_data_helper` решает сразу несколько задач:

- читает данные из SAP (много `SELECT` в одном методе),
- маппит данные в структуры XML (Header/Items/Podmiot*),
- парсит старый XML (коррекция) и сравнивает структуры,
- формирует «старые значения» для секций `Podmiot1K`, `Podmiot2K`, `FaWiersz` и `ZamowienieWiersz`,
- извлекает одиночные теги вроде `P_15`, `KursWalutyZ` из XML.

Это нарушает слойность и делает класс трудно тестируемым: невозможно изолировать селекты, парсинг XML и правила сравнения.

Цель рефакторинга — привести реализацию к архитектуре из документа: **UI → Orchestrator → Foundation (XML Service + DB Repository) → Infrastructure** fileciteturn6file0.

---

## 2. Целевое место в архитектуре (Layers)

### 2.1 Что должно остаться в UI/Orchestrator
- UI (cockpit) передаёт **список `ksef_id`** (пакетно), а не BELNR/GJAHR/BUKRS напрямую.
- Orchestrator **координирует**: получает данные, вызывает XML service, пишет статусы/логи через repository+log_manager.

### 2.2 Что должно быть в Foundation

#### A) `ZCL_KSEF_FOUND_DB_REPOSITORY`
- Пакетные чтения: заголовки, позиции, партнёры, условия оплаты, ссылки на корректируемый документ, XML предыдущей версии и т.п.
- Никакого XML/бизнес-правил — только CRUD/селекты/джойны.

#### B) `ZCL_KSEF_FOUND_XML_SERVICE`
- Генерация структур для XML (внутренний invoice model).
- Генерация XML-строки и валидации (XSD + бизнес-валидация).
- Парсинг предыдущего XML для нужных секций (подмножества), чтобы сделать корректировку.
- Возврат результата в формате batch.

### 2.3 Что переносим из `zcl_ksef_data_helper`
- Вся логика **парсинга XML**, **диффов**, **формирования данных для KOR** — в XML Service (или его внутренние helper-классы).
- Все селекты — в DB Repository или отдельный “data provider” в Foundation (но лучше в repository, как в архитектуре). fileciteturn6file0

---

## 3. Ключевой контракт: batch по `ksef_id`

### 3.1 Вход/выход XML Service (предлагаемый интерфейс)

```abap
TYPES:
  BEGIN OF ty_xml_request,
    ksef_id TYPE zlx_ksef_id,
  END OF ty_xml_request,
  tt_xml_request TYPE STANDARD TABLE OF ty_xml_request WITH EMPTY KEY.

TYPES:
  BEGIN OF ty_xml_result,
    ksef_id     TYPE zlx_ksef_id,
    xml_string  TYPE string,
    status      TYPE char1,          " S/F
    messages    TYPE zkstg_t_message, " единый формат сообщений
  END OF ty_xml_result,
  tt_xml_result TYPE STANDARD TABLE OF ty_xml_result WITH KEY ksef_id.

METHODS create_and_validate_xmls
  IMPORTING it_ksef_ids TYPE zkstg_t_inv_key
            io_logger   TYPE REF TO zif_ksef_log_manager
  RETURNING VALUE(rt_results) TYPE tt_xml_result
  RAISING zcx_ksef_xml_error.
```

**Почему так:**
- UI/Orchestrator всегда работает с `ksef_id` (как в архитектуре) fileciteturn6file0.
- batch даёт производительность: один набор селектов + цикл обработки.
- результат по каждому инвойсу независим: можно частично успешно обработать пакет.

---

## 4. Внутренняя модель данных (invoice domain model)

Чтобы не протаскивать “сырые” структуры (типа `zksef_s_head`, `zksef_s_item`, `zlx_ksef_podmiot*`) через слои, вводим **внутреннюю модель**, отражающую домен “инвойс для KSeF”, например:

- `ty_invoice_header`
- `ty_invoice_item[]`
- `ty_podmiot` (унифицировано для Podmiot1/2/3/2K/1K)
- `ty_zal_item[]` (ZamowienieWiersz)
- `ty_correction_context` (старый XML, номера, причины и т.д.)

**Важно:** UI-структуры (`zksef_s_head`, ALV-структуры, type-pool `zkstg_*`) остаются на уровне UI/презентации. XML Service отдаёт XML и сообщения.

---

## 5. Разделение обязанностей внутри XML Service

Рекомендую внутри `ZCL_KSEF_FOUND_XML_SERVICE` выделить **внутренние компоненты** (локальные классы/или отдельные классы Foundation):

### 5.1 Data Assembler (из данных репозитория → domain model)
- `lcl_invoice_assembler`  
- строит `ty_invoice_model` из данных, полученных из repository.
- не парсит XML, не делает diff.

### 5.2 XML Parser (строго чтение XML → структуры)
- `lcl_xml_reader`  
- методы:
  - `read_podmiot( iv_xml, iv_tagname ) -> rs_podmiot`
  - `read_podmiot3_list( iv_xml ) -> rt_podmiot`
  - `read_items( iv_xml ) -> rt_items`
  - `read_zal_items( iv_xml ) -> rt_zal_items`
  - `read_simple_tag( iv_xml, iv_tagname ) -> rv_value` (для P_15, KursWalutyZ и т.д.)

**Улучшение:** отказаться от “двух тегов за раз” (`parse_tag_from_xml`), сделать универсальный `read_simple_tag`. При необходимости — “multi-tag” как обертка, но на основе `read_simple_tag`.

### 5.3 Diff Engine (сравнение)
- `lcl_diff`  
- `diff_podmiot( a, b ) -> abap_bool`
- `diff_items( old, new ) -> abap_bool`
- `diff_zal_items( old, new ) -> abap_bool`
- сравнение должно быть:
  - единообразным (один подход для всех сущностей),
  - устойчивым к пустым/initial значениям,
  - при необходимости учитывать “нормализацию” (trim, формат чисел).

#### Правила сравнения (FA(3), корректировки)
- **Нормализация сумм**: суммы и количества сравниваются как числа (decfloat/p), а не строки.
  Используется допуск (tolerance) по умолчанию `0.01` для полей сумм (например, `P_15` и поля
  сумм по позициям).  
- **Compare modes**:
  - `FULL` (по умолчанию): сравниваются все поля, суммы с нормализацией.
  - `TOTALS_ONLY`: сравниваются только числовые/суммовые поля; различия по тексту игнорируются.
- **Позиции (FaWiersz/ZamowienieWiersz)**:
  - при наличии стабильного ключа сравнение выполняется по ключу (`UU_ID`/`NrWiersza*`),
  - при отсутствии ключа — по индексу (с документированием допущения в коде).
- **Podmiot3**:
  - при наличии ключа (роль + NIP/ID) сравнение выполняется по ключу,
  - иначе — по индексу (допущение фиксируется в коде).
- **Missing vs Initial**: отсутствие старых данных трактуется как изменение (по умолчанию),
  но форматные различия (пробелы, лидирующие нули в числовых строках) не считаются изменением.

### 5.4 Correction Builder (формирование данных для KOR)
- `lcl_correction_builder`
- правила:
  - `Podmiot1K`: старый Podmiot1 если отличается от нового.
  - `Podmiot2K`: **табличная секция**:
    1) старый Podmiot2 (если отличается),
    2) затем по очереди старые Podmiot3, но **только для тех индексов**, где Podmiot3_new отличается от Podmiot3_old,
       а если новый Podmiot3 отсутствует — старый можно добавить (как “removed/changed”).
  - `FaWiersz` и `ZamowienieWiersz`: если есть изменения, выводить пары (old+new) с `StanPrzed=1` у old.
  - Correction Builder опирается **только** на результат diff engine (ключи/флаги),
    не выполняет повторное сравнение и не добавляет пустые `*_K` секции.

Эта логика у вас уже наметилась в `fill_pod2k`, `fill_items_kor`, `fill_zal_items_kor` — её стоит сгруппировать и покрыть тестами.

---

### 5.5 Текущее состояние (скелет)

В репозитории создан скелет фасада `ZCL_KSEF_FOUND_XML_SERVICE` с batch-API по `ksef_id` и отдельными классами-компонентами (пустые реализации):

- `ZCL_KSEF_FOUND_XML_REPOSITORY` (заглушка без SELECT),
- `ZCL_KSEF_FOUND_XML_ASSEMBLER`,
- `ZCL_KSEF_FOUND_XML_READER`,
- `ZCL_KSEF_FOUND_XML_DIFF`,
- `ZCL_KSEF_FOUND_XML_COR_BUILDER`,
- `ZCL_KSEF_FOUND_XML_RENDERER`,
- `ZCL_KSEF_FOUND_XML_VALIDATOR`.

Скелет отражает распределение обязанностей и служит точкой для дальнейшего переноса логики из `zcl_ksef_data_helper` без изменения поведения существующего кода.

---

### 5.6 Renderer determinism rules (FA(3))

Renderer (`ZCL_KSEF_FOUND_XML_RENDERER`) must produce **deterministic** XML output for FA(3):

**Ordering rules**
- Repeating sections (`FaWiersz`, `ZamowienieWiersz`, `Podmiot3`, `Podmiot2K`) are rendered in a **stable, explicit order**.
- If input tables are hashed or non-ordered, renderer **must sort** them into a stable order **before** rendering.
- Sorting keys are derived from stable business identifiers (e.g. `UU_ID`, `NrWiersza*`, party role + ID). If keys are missing, a deterministic structural key is used.

**Formatting flag**
- Pretty-printing/indentation is controlled by a **single flag** (`pretty_print`) in renderer options.
- Default is **off** (raw, compact output) unless the project explicitly decides otherwise.
- No random whitespace differences are introduced between runs.

**Namespaces & envelope**
- Root structure uses FA(3) envelope: `<Faktura>` with **single** canonical namespace set.
- Namespace values are fixed and consistent across outputs (no variants).
- Namespace definitions must not be duplicated or reordered dynamically.

---

## 6. Рефакторинг `zcl_ksef_data_helper`

### 6.1 Что делать с классом
Варианты (в порядке предпочтения):

1) **Удалить как публичный API**, оставив только оркестратор+XML Service.  
   Текущие методы становятся приватными/lcl внутри XML Service.

2) Переименовать и сузить ответственность:  
   `ZCL_KSEF_FOUND_XML_HELPER` (только парсинг/дифф/коррекция), а селекты и сборка модели — в XML Service.

**В обоих случаях:** класс не должен быть точкой входа UI.

### 6.2 Привязка к `ksef_id`
- В UI хранится `ksef_id`.
- Repository умеет по `ksef_id` вернуть:
  - связку BELNR/GJAHR/BKPF/BSEG/VBRK/VBRP и т.д. (что нужно),
  - “корректируемый” XML (`zlx_ksef_out-xml`) и ksef_num/xmlgen_date предыдущего документа,
  - список партнёров (Podmiot3) и их роли.

### 6.3 Селекты вынести и “пакетировать”
- вместо множества `SELECT SINGLE` на каждый инвойс — один пакетный отбор:
  - сначала список ключей,
  - затем `FOR ALL ENTRIES IN`/joins/CTE (в зависимости от версии) по нужным таблицам,
  - затем сборка по `ksef_id` в хеш-таблицы для O(1) доступа в цикле.
- это критично для cockpit (массовая генерация XML). fileciteturn6file0

---

## 7. Типы и type-pool `zkstg`

Ваше предложение вынести типы в `TYPE-POOL zkstg` — хорошее, но важно не “перемешать слои”:

- **UI/ALV типы** (`zkstg_header`, `zkstg_top_alv*`, фильтры, сообщения) — да, в type-pool.
- **domain model для XML** — лучше как TYPES внутри XML service (или отдельный include/type group внутри Foundation), чтобы UI не зависел от внутренних структур генерации XML.
- **shared XML Service types** — определены в `ZIF_KSEF_XML_TYPES`.
- **сообщения** — хорошо унифицировать: `zkstg_s_message` / `zkstg_t_message` как единый формат.

---

## 8. Логирование и ошибки

Согласно архитектуре fileciteturn6file0:

- XML операции логируем в `KSEFOUT/XML/<ksef_id>`.
- В `create_and_validate_xmls`:
  - на старт инвойса: `OVERALL` + `XML` лог,
  - на каждый шаг: сбор данных, генерация XML, XSD, бизнес-валидация,
  - при ошибке: подробности в XML-log, короткое summary в OVERALL-log.

Ошибки:
- внутри XML service бросаем `ZCX_KSEF_XML_ERROR` (с `previous` и деталями).
- наружу в orchestrator возвращаем per-invoice `messages`, чтобы UI мог показать без dump.

### 8.1 Бизнес-валидация FA(3)

Бизнес-валидация выполняется в `ZCL_KSEF_FOUND_XML_VALIDATOR` и возвращает сообщения
в `ZIF_KSEF_XML_TYPES=>tt_message`, который прокидывается в `tt_xml_result-messages`.
Каждое сообщение содержит:

- `severity` (E/W/I),
- `code` (стабильный идентификатор правила),
- `text` (человеко-читаемый текст),
- контекст (`ksef_id`, `item_no`, `field_name`) при наличии.

Категории правил (минимально безопасный набор):

1. **Обязательные поля заголовка** — идентификаторы/даты/валюта/тип документа FA(3).
2. **Стороны (Podmiot*)** — обязательные идентификаторы и согласованность данных.
3. **Позиции** — наличие хотя бы одной позиции, обязательные поля, числовые форматы.
4. **Итоги** — наличие итогов и согласованность сумм с позициями (с допуском).
5. **Корректировки** — обязательные ссылки на исправляемый документ.

Все ошибки валидатора логируются в BAL `KSEFOUT/XML/<ksef_id>`.

---

## 9. Тестируемость

Рекомендации:

- `lcl_xml_reader` тестировать на статичных XML-строках (fixture).
- `lcl_diff` тестировать на структурах (без XML).
- `lcl_correction_builder` тестировать сценариями:
  - Podmiot2 изменился,
  - Podmiot3[1] изменился, Podmiot3[2] нет,
  - Podmiot3 удалён/добавлен,
  - items changed / not changed,
  - zal_items changed / not changed.
- Repository покрыть интеграционными тестами (или хотя бы тест-программой ZKSEF_TEST_*). fileciteturn6file0

---

## 10. План миграции (без “большого взрыва”)

1) Создать `ZCL_KSEF_FOUND_DB_REPOSITORY` методы чтения для вашего текущего набора данных.
2) Создать `ZCL_KSEF_FOUND_XML_SERVICE` со “скелетом” batch API.
3) Перенести парсинг XML (podmiot/items/zal_items/tag) в `lcl_xml_reader`.
4) Перенести diff и builders (Podmiot1K/Podmiot2K/items/zal_items) в отдельные helper’ы.
5) В orchestrator заменить вызовы `zcl_ksef_data_helper->fill_table` на:
   - repository->get_data( ksef_id )
   - xml_service->create_and_validate_xmls( it_ksef_ids )
6) После стабилизации удалить/депрекейтить старый helper.

---

## 11. Короткий checklist соответствия архитектуре

- [ ] Публичный вход в генерацию XML — только через orchestrator (`ZCL_KSEF_ORCH_XML_CREATE`) и XML service (`ZCL_KSEF_FOUND_XML_SERVICE`) fileciteturn6file0  
- [ ] Селекты вынесены в repository, пакетные  
- [ ] `ksef_id` — основной ключ  
- [ ] Логи: MAIN/OVERALL/XML в BAL через `ZIF_KSEF_LOG_MANAGER`  
- [ ] Ошибки: `ZCX_KSEF_XML_ERROR` + per-invoice messages  
- [ ] Тесты на parser/diff/builder

---

## 12. Приложение: рекомендации по вашему текущему коду

### 12.1 `fill_pod2k`
- В текущей версии вы сравниваете Podmiot3 “по индексу”. Это нормально, но:
  - добавьте сравнение по ключу, если появится стабильный идентификатор (например NIP+rola), иначе индекс — ок.
  - отделите “маппинг zlx_ksef_podmiot3 → zlx_ksef_podmiot” в отдельную функцию (normalize).
  - фиксируйте кейсы “новый есть, старого нет” / “старый есть, нового нет” сообщениями уровня W.

### 12.2 `parse_*_from_xml`
- Уберите логику, связанную с префиксами (вы уже уточнили, что их нет) — это снижает шум и риск ошибок.
- Делайте `CONDENSE`/`SHIFT` для текстов, если источники могут содержать пробелы/переносы.

### 12.3 `items_differs`
- Сейчас сравниваются поля “как есть”. Для сумм и процентов лучше нормализовать типы:
  - строка → decfloat34/packed с округлением до нужной точности,
  - иначе дифф может срабатывать из-за формата.

### 12.4 `components/contracts`
  Cross-component DTO/types are defined in shared public contracts (interfaces/types), not in private sections.

---

**Конец документа.**
