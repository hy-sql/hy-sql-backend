const Joi = require('@hapi/joi')
const OrderBySchema = require('./OrderBySchema')
const WhereSchema = require('./WhereSchema')
const {
    AllFieldsSchema,
    ColumnSchema,
    TextSchema,
    IntegerSchema,
} = require('./FieldSchemas')
const ExpressionSchema = require('./ExpressionSchema')
const FunctionSchema = require('./FunctionSchema')

/**
 * Joi schema for validating SELECT commands not containing WHERE or ORDER BY.
 */
const SelectSchema = Joi.object({
    name: Joi.string().required().valid('SELECT').insensitive().messages({
        'any.only': 'Query must begin with SELECT *',
        'any.required': 'Query must begin with SELECT *',
    }),

    fields: Joi.array()
        .items(
            AllFieldsSchema,
            ColumnSchema,
            TextSchema,
            IntegerSchema,
            ExpressionSchema.shared(FunctionSchema),
            FunctionSchema.shared(ExpressionSchema)
        )
        .min(1)
        .required(),

    from: Joi.string()
        .required()
        .pattern(/;/, { invert: true })
        .pattern(/^FROM$/i)
        .messages({
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base': 'SELECT * must be followed by FROM',
            'any.required': 'SELECT * must be followed by FROM',
        }),

    tableName: Joi.string().required().pattern(/^\w+$/).max(64).messages({
        'any.required': 'Query must contain a table name',
        'string.pattern.base':
            'Table name should only contain one or more alphanumeric characters and underscores',
        'string.max': 'The table name is too long',
    }),

    finalSemicolon: Joi.string().required().valid(';').messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),

    additional: Joi.array().max(0),
})

/**
 * Joi schema for validating SELECT commands containing WHERE but not ORDER BY.
 */
const SelectWhereSchema = SelectSchema.keys({
    where: WhereSchema,
})

/**
 * Joi schema for validating SELECT commands containing ORDER BY but not WHERE.
 */
const SelectOrderBySchema = SelectSchema.keys({
    orderBy: OrderBySchema,
})

/**
 * Joi schema for validating SELECT commands containing WHERE and ORDER BY.
 */
const SelectWhereOrderBySchema = SelectSchema.keys({
    where: WhereSchema,
    orderBy: OrderBySchema,
})

module.exports = {
    SelectSchema,
    SelectOrderBySchema,
    SelectWhereSchema,
    SelectWhereOrderBySchema,
}
