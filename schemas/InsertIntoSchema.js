const Joi = require('@hapi/joi')
const { ColumnSchema, TextSchema, IntegerSchema } = require('./FieldSchemas')

/**
 * Joi schema for validating INSERT INTO commands.
 */
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

    columns: Joi.array().min(1).items(ColumnSchema).required().messages({
        'array.base': 'this is not an array',
        'array.min': 'there should be at least one column specified',
        'any.required': 'There should be at least one column specified',
    }),

    valuesKeyword: Joi.string()
        .valid('VALUES')
        .insensitive()
        .required()
        .messages({
            'any.only':
                'The keyword VALUES is either misspelled, missing or in the wrong position',
            'any.required':
                'This query is expected to contain the following keyword: VALUES',
        }),

    values: Joi.array()
        .items(TextSchema, IntegerSchema)
        .min(Joi.ref('columns.length'))
        .max(Joi.ref('columns.length'))
        .required()
        .messages({
            'array.base': 'this is not an array',
            'array.min': 'Amount of values must match amount of columns',
            'array.max': 'Amount of values must match amount of columns',
            'any.required': 'There should be at least one value specified',
        }),

    finalSemicolon: Joi.string().valid(';').required().messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

module.exports = { InsertIntoSchema }
