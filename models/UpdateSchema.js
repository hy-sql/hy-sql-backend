const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    columnName: Joi.string()
        .required()
        .pattern(/;/, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .messages({
            'any.required': 'SET should be followed by column name',
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base':
                'Number and string values in the query should only contain alphanumeric characters and underscores',
            'string.max': 'A string value is too long',
        }),
    sign: Joi.boolean().required().valid(true).messages({
        'any.only': 'After SET there should be = sign',
        'any.required': 'After SET there should be = sign',
    }),
    valueType: Joi.string().valid('INTEGER', 'TEXT').insensitive().required(),
    value: Joi.alternatives().conditional('valueType', {
        is: 'INTEGER',
        then: Joi.number().required().messages({
            'any.required':
                'value type without singlequotes is expected to be number',
        }),
        otherwise: Joi.string()
            .required()
            .pattern(/;/, { invert: true })
            .pattern(/^\w+$/)
            .max(64)
            .messages({
                'any.required':
                    'If singlequotes are given value should be a string',
                'string.pattern.invert.base':
                    'Semicolon should only be found at the end of a query',
                'string.pattern.base':
                    'Number and string values in the query should only contain alphanumeric characters and underscores',
                'string.max': 'A string value is too long',
            }),
    }),
})

const UpdateSchema = Joi.object({
    name: Joi.string()
        .pattern(/[;]./, { invert: true })
        .required()
        .valid('UPDATE')
        .insensitive()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'any.only': 'Query must begin with UPDATE',
            'any.required': 'Query must begin with UPDATE',
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

    set: Joi.string().required().valid('SET').insensitive().messages({
        'any.required': 'Query must contain SET',
    }),

    columns: Joi.array().min(1).items(ColumnsSchema).required().messages({}),

    //WHERE-parsiminen tähän TODO

    finalSemicolon: Joi.string().required().valid(';').messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

module.exports = { UpdateSchema }
