const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    name: Joi.string().pattern(/^\w+$/).max(64).required().messages({
        'string.pattern.base':
            'One of the column names is invalid. Only a-z,A-Z,0-9 and _ allowed.',
        'string.max': 'One of the column names is too long',
        'any.required': 'A column name is missing',
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
        then: Joi.number().required().messages({
            'any.required': 'Missing a value to be set',
        }),
        otherwise: Joi.string()
            .required()
            .pattern(/^\w+$/)
            .min(1)
            .max(64)
            .messages({
                'string.pattern.base':
                    'One of the specified values is invalid. Only a-z,A-Z,0-9 and _ allowed.',
                'string.max': 'One of the specified values is too long',
                'any.required': 'A value is missing',
            }),
    }),

    type: Joi.string()
        .valid('INTEGER', 'TEXT')
        .insensitive()
        .required()
        .messages({}),
})

const InsertIntoSchema = Joi.object({
    name: Joi.string()
        .valid('INSERT INTO')
        .insensitive()
        .required()
        .min(3)
        .max(30)
        .messages({
            'any.only': 'Query must begin with INSERT INTO',
            'any.required': 'Query must begin with INSERT INTO',
        }),

    tableName: Joi.string()
        .required()
        .pattern(/^\w+$/)
        .min(1)
        .max(64)
        .messages({
            'string.base': 'this is not a string',
            'any.required': 'Query must contain a table name',
            'string.pattern.base':
                'Table name is invalid. Only a-z,A-Z,0-9 and _ allowed.',
            'string.max': 'The table name is too long',
        }),

    size: Joi.number().positive().required().messages({}),

    parserCounter: Joi.number()
        .required()
        //.min(Joi.ref('size'))
        .messages({}),

    columnsOpeningBracket: Joi.string().valid('(').required().messages({
        'any.only': '( expected at the beginning of the list of columns',
        'any.required': '( expected at the beginning of the list of columns',
    }),

    columns: Joi.array().min(1).items(ColumnsSchema).required().messages({
        'array.base': 'this is not an array',
        'array.min': 'there should be at least one column specified',
        'any.required': 'There should be at least one column specified',
    }),

    columnsClosingBracket: Joi.string().valid(')').required().messages({
        'any.only': ') expected at the end of the list of columns',
        'any.required': ') expected at the end of the list of columns',
    }),

    anchorKeyword: Joi.string()
        .valid('VALUES')
        .insensitive()
        .required()
        .messages({
            'any.only':
                'The keyword VALUES is either misspelled, missing or in the wrong position',
            'any.required':
                'This query is expected to contain the following keyword: VALUES',
        }),

    valuesOpeningBracket: Joi.string().valid('(').required().messages({
        'any.only': '( expected at the beginning of the list of values',
        'any.required': '( expected at the beginning of the list of values',
    }),

    values: Joi.array()
        .items(ValuesSchema)
        .min(Joi.ref('columns.length')) //jos näitä on enemmän kuin kenttiä, ylimääräisten column on undefined => validaatiovirhe
        .required()
        .messages({
            'array.base': 'this is not an array',
            'array.min': 'there should be at least one value',
            'any.required': 'There should be at least one value specified',
        }),

    valuesClosingBracket: Joi.string().valid(')').required().messages({
        'any.only': ') expected at the end of the list of values',
        'any.required': ') expected at the end of the list of values',
    }),

    finalSemicolon: Joi.string().valid(';').required().messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

module.exports = { InsertIntoSchema }
