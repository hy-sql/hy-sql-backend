const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    name: Joi.string().pattern(/^\w+$/).max(64).required().messages({
        'string.pattern.base':
            'One of the column names is invalid. Only a-z,A-Z,0-9 and _ allowed.',
    }),
})

const ValuesSchema = Joi.object({
    column: Joi.string()
        .pattern(/^\w+$/)
        .min(1)
        .max(64)
        .required()
        .messages({}),

    value: Joi.alternatives().conditional('type', {
        is: 'INTEGER',
        then: Joi.number(),
        otherwise: Joi.string()
            .pattern(/^\w+$/)
            .min(1)
            .max(64)
            .required()
            .messages({}),
    }),

    type: Joi.string()
        .valid('INTEGER', 'TEXT')
        .insensitive()
        .required()
        .messages({}),
})

const InsertIntoSchema = Joi.object({
    name: Joi.string()
        .min(3)
        .max(30)
        .valid('INSERT INTO')
        .insensitive()
        .required()
        .messages({
            'any.only': 'Command must be "INSERT INTO"',
        }),

    tableName: Joi.string()
        .pattern(/^\w+$/)
        .min(2)
        .max(64)
        .required()
        .messages({
            'string.base': 'this is not a string',
        }),

    columnsOpeningBracket: Joi.string().valid('(').required(),

    columns: Joi.array().min(1).items(ColumnsSchema).required().messages({
        'array.base': 'this is not an array',
        'array.min': 'there should be at least one column',
    }),

    columnsClosingBracket: Joi.string().valid(')').required(),

    anchorKeyword: Joi.string().valid('VALUES').insensitive().required(),

    valuesOpeningBracket: Joi.string().valid('(').required(),

    values: Joi.array()
        .items(ValuesSchema)
        .min(Joi.ref('columns.length')) //jos näitä on enemmän kuin kenttiä, ylimääräisten column on undefined => validaatiovirhe
        .required()
        .messages({
            'array.base': 'this is not an array',
            'array.min': 'there should be at least one value',
        }),

    valuesClosingBracket: Joi.string().valid(')').required(),

    finalSemicolon: Joi.string().valid(';').required(),
})

module.exports = { InsertIntoSchema }
