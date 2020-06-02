const Joi = require('@hapi/joi')
const { constraintsPattern } = require('../utils/regex')

const ColumnsSchema = Joi.object({
    name: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .required()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'string.pattern.base':
                'Only letters, numbers and underscore are allowed in column names',
            'string.max': 'One of the column names is too long',
            'any.required': 'A column name is missing',
        }),

    type: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^INTEGER$|^TEXT$/)
        .insensitive()
        .required()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'string.pattern.base': 'Invalid type',
            'any.required': 'A type must be specified for all columns',
        }),

    constraints: Joi.array().items(
        Joi.string()
            .pattern(/[;]/, { invert: true })
            .pattern(constraintsPattern)
            .insensitive()
            .optional()
            .messages({
                'string.pattern.invert.base':
                    'Semicolon should be only found at the end of a query',
                'string.pattern.base': 'Invalid constraint or comma missing',
            })
    ),
})

const CreateTableSchema = Joi.object({
    name: Joi.string()
        .pattern(/[;]./, { invert: true })
        .valid('CREATE TABLE')
        .insensitive()
        .required()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'any.only': 'Query must begin with CREATE TABLE',
            'any.required': 'Query must begin with CREATE TABLE',
        }),

    tableName: Joi.string()
        .pattern(/;/, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .required()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'string.pattern.base':
                'Table name should only contain one or more alphanumeric characters and underscores',
            'string.max': 'The table name is too long',
            'any.required': 'Query must contain a table name',
        }),

    openingBracket: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^\($/)
        .required()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'string.pattern.base':
                '( expected between the table name and the list of columns',
            'any.required':
                '( expected between the table name and the list of columns',
        }),

    columns: Joi.array().items(ColumnsSchema).required().messages({
        'any.required': 'There should be at least one column specified',
    }),

    closingBracket: Joi.boolean().valid(true).required().messages({
        'any.only': ') expected at the end of the list of columns',
        'any.required': ') expected at the end of the list of columns',
    }),

    finalSemicolon: Joi.string().valid(';').required().messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

module.exports = { CreateTableSchema, ColumnsSchema }
