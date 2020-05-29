const Joi = require('@hapi/joi')

/* WhereSchema expects the following keyes; keyword, columnName, sign, valueType and value.
  Of these the valueType must be added when parsing to indicate wheter the value is an integer or a string,
  to indicate on what rules it should be validated. The valueType must be either 'TEXT' or 'INTEGER'. If value is
  undefined valueType should default to 'INTEGER'.
  The validation rules for a value are: an integer must be a number, a string must be surrounded by quotes and
  only contain underscores and alphanumeric characters.
*/

const WhereSchema = Joi.object({
    keyword: Joi.string()
        .required()
        .pattern(/;/, { invert: true })
        .pattern(/^([Ww][Hh][Ee][Rr][Ee])$/)
        .messages({
            'any.required':
                'This query is expected to have the following keyword: WHERE',
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base':
                'WHERE is either misspelled or in the wrong position',
        }),

    columnName: Joi.string()
        .required()
        .pattern(/;/, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .messages({
            'any.required': 'WHERE must be followed by a column name',
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base':
                'Column names should only contain one or more alphanumeric characters and underscores',
            'string.max': 'Column name is too long',
        }),

    sign: Joi.string().required().valid('=', '<=', '>=', '>', '<').messages({
        'any.only':
            'After WHERE, column name and the value must be separated by: =, <=, >=, > or <',
        'any.required':
            'After WHERE, column name and the value must be separated by: =, <=, >=, > or <',
    }),

    valueType: Joi.string().valid('INTEGER', 'TEXT').insensitive().required(),

    value: Joi.alternatives().conditional('valueType', {
        is: 'INTEGER',
        then: Joi.number().required().messages({
            'any.required':
                'After WHERE, a value must be defined after the column name',
        }),
        otherwise: Joi.string()
            .required()
            .pattern(/;/, { invert: true })
            .pattern(/^\w+$/)
            .max(64)
            .messages({
                'any.required':
                    'After WHERE, a value must be defined after the column name',
                'string.pattern.invert.base':
                    'Semicolon should only be found at the end of a query',
                'string.pattern.base':
                    'Number and string values in the query should only contain alphanumeric characters and underscores',
                'string.max': 'A string value is too long',
            }),
    }),

    indexCounter: Joi.number(),
})

module.exports = WhereSchema
