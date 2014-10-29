class SchemaError(Exception):
  pass

class ParseError(Exception):
  def __init__(self, message):
    super().__init__(message)
    self.ok_message = message
    self.sections = []

  def __str__(self):
    if self.sections:
      return 'While validating section "{}": {}'.format('.'.join(self.sections), self.ok_message)
    return self.ok_message


class Type(object):
  @classmethod
  def _validate(cls, structure):
    raise NotImplementedError

class Section(Type):
  @classmethod
  def _validate(cls, structure):
    if not isinstance(structure, dict):
      raise ParseError('Expected section, but got {}: {}.'.format(structure.__class__.__name__, structure))

    for field, subschema in cls.__dict__.items():
      if field.startswith('_'):
        continue

      try:
        substructure = structure[field]
      except KeyError:
        if isinstance(subschema, optional):
          substructure = subschema.default
        else:
          raise ParseError('Required field "{}" not found.'.format(field))
      else:
        if isinstance(subschema, optional):
          subschema = subschema.type
        try:
          substructure = validate(substructure, subschema)
        except ParseError as e:
          e.sections.insert(0, field)
          raise

      structure[field] = substructure
    return structure

class anything(Type):
  @classmethod
  def _validate(cls, structure):
    return structure

class constrained(Type):
  def __init__(self, type, constraint):
    self.type = type
    self.constraint = constraint

  def _validate(self, structure):
    structure = validate(structure, self.type)
    ok, message = self.constraint(structure)
    if not ok:
      raise ParseError('Constraint failure: {}: {}.'.format(message, structure))
    return structure

class optional(object):
  def __init__(self, type, default=None):
    self.type = type
    self.default = default

def any(values):
  def check_any(structure):
    if structure in values:
      return (True, None)
    message = "must be any of {}".format(', '.join(repr(x) for x in values))
    return (False, message)

  return constrained(anything, check_any)


def validate(structure, schema):
  if isinstance(schema, dict):
    if not isinstance(structure, dict):
      raise ParseError('Expected dict, but got {}: {}.'.format(structure.__class__.__name__, structure))
    if len(schema) != 1:
      raise SchemaError('Expected dict with a single element as type, but dict with {} items.'.format(len(schema)))

    kschema, vschema = next(iter(schema.items()))
    for k, v in structure.items():
      try:
        k = validate(k, kschema)
        v = validate(v, vschema)
      except ParseError as e:
        e.sections.insert(0, '[{!r}]'.format(k))
        raise

      structure[k] = v
  elif isinstance(schema, list):
    if not isinstance(structure, list):
      raise ParseError('Expected list, but got {}: {}.'.format(structure.__class__.__name__, structure))
    if len(schema) != 1:
      raise SchemaError('Expected list with a single element as type, but got list with {} items.'.format(len(schema)))

    subschema = schema[0]
    for i in range(len(structure)):
      try:
        structure[i] = validate(structure[i], subschema)
      except ParseError as e:
        e.sections.insert(0, '[{}]'.format(i))
        raise
  elif isinstance(schema, Type) or issubclass(schema, Type):
    structure = schema._validate(structure)
  elif issubclass(schema, str):
    if not isinstance(structure, str):
      if isinstance(structure, bytes):
        structure = structure.decode('utf-8')
      else:
        raise ParseError('Expected string, but got {}: {}.'.format(structure.__class__.__name__, structure))
  elif issubclass(schema, int):
    if not isinstance(structure, int):
      raise ParseError('Expected integer, but got {}: {}.'.format(structure.__class__.__name__, structure))
  elif issubclass(schema, bool):
    if not isinstance(structure, bool):
      raise ParseError('Expected bool, but got {}: {}.'.format(structure.__class__.__name__, structure))
  else:
    raise SchemaError('Unknown validation type in schema: {}'.format(schema))

  return structure
