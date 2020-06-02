const constraintsPatternForSplit = new RegExp(
    [
        '(?<=PRIMARY KEY)|(?=PRIMARY KEY)',
        '(?<=CHECK)|(?=CHECK)',
        '(?<=NOT NULL)|(?=NOT NULL)',
        '(?<=UNIQUE)|(?=UNIQUE)',
        '(?<=FOREIGN KEY)|(?=FOREIGN KEY)',
        '(?<=INDEX)|(?=INDEX)',
    ].join('|')
)

const constraintsPattern = new RegExp(
    ['CHECK', 'NOT NULL', 'UNIQUE', 'PRIMARY KEY', 'FOREIGN KEY', 'INDEX'].join(
        '|'
    )
)

const stringFunctionsPattern = new RegExp(
    ['LENGTH', 'CONCAT', 'SUBSTRING'].join('|')
)

const aggregateFunctionsPattern = new RegExp(
    ['AVG', 'COUNT', 'MAX', 'MIN', 'SUM'].join('|')
)

const arithmeticOperatorPattern = RegExp('[+|-|*|/|%]', 'g')

const comparisonOperatorPattern = RegExp('[=|>|<|>=|<=|<>]', 'g')

const logicalOperatorPattern = RegExp(
    '[ALL|AND|ANY|BETWEEN|EXISTS|IN|LIKE|NOT|OR|SOME]',
    'g'
)

module.exports = {
    constraintsPatternForSplit,
    constraintsPattern,
    stringFunctionsPattern,
    aggregateFunctionsPattern,
    arithmeticOperatorPattern,
    comparisonOperatorPattern,
    logicalOperatorPattern,
}
