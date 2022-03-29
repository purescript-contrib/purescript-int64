var L = require("long");

// Constants
exports.zero     = L.ZERO;
exports.one      = L.ONE;
exports.negOne   = L.NEG_ONE;
exports.uzero    = L.UZERO;
exports.uone     = L.UONE;
exports.maxValue = L.MAX_VALUE;
exports.maxUnsignedValue = L.MAX_UNSIGNED_VALUE;
exports.minValue = L.MIN_VALUE;

// Utilities
exports.isLong = L.isLong;
exports.fromBits = L.fromBits;
exports.fromBytes = L.fromBytes;
exports.fromBytesLE = L.fromBytesLE;
exports.fromBytesBE = L.fromBytesBE;
exports.fromInt = L.fromInt;
exports.fromNumber = L.fromNumber;
exports.fromString = L.fromString;
exports.fromValue = L.fromValue;

// Fields
exports.unsigned = function (l) { return l.unsigned; };

// Methods
exports.add = function (l) { return l.add.bind(l); };
exports.and = function (l) { return l.and.bind(l); };
exports.compare = function (l) { return l.compare.bind(l); };
exports.divide = function (l) { return l.divide.bind(l); };
exports.equals = function (l) { return l.equals.bind(l); };
exports.getHighBits = function (l) { return l.getHighBits(); };
exports.getHighBitsUnsigned = function (l) { return l.getHighBitsUnsigned(); };
exports.getLowBits = function (l) { return l.getLowBits(); };
exports.getLowBitsUnsigned = function (l) { return l.getLowBitsUnsigned(); };
exports.getNumBitsAbs = function (l) { return l.getNumBitsAbs(); };
exports.greaterThan = function (l) { return l.greaterThan.bind(l); };
exports.greaterThanOrEqual = function (l) { return l.greaterThanOrEqual.bind(l); };
exports.isEven = function (l) { return l.isEven(); };
exports.isNegative = function (l) { return l.isNegative(); };
exports.isOdd = function (l) { return l.isOdd(); };
exports.isPositive = function (l) { return l.isPositive(); };
exports.isZero = function (l) { return l.isZero(); };
exports.lessThan = function (l) { return l.lessThan.bind(l); };
exports.lessThanOrEqual = function (l) { return l.lessThanOrEqual.bind(l); };
exports.modulo = function (l) { return l.modulo.bind(l); };
exports.multiply = function (l) { return l.multiply.bind(l); };
exports.negate = function (l) { return l.negate(); };
exports.not = function (l) { return l.not(); };
exports.notEquals = function (l) { return l.notEquals.bind(l); };
exports.or = function (l) { return l.or.bind(l); };
exports.shiftLeft = function (l) { return l.shiftLeft.bind(l); };
exports.shiftRight = function (l) { return l.shiftRight.bind(l); };
exports.shiftRightUnsigned = function (l) { return l.shiftRightUnsigned.bind(l); };
exports.rotateLeft = function (l) { return l.rotateLeft.bind(l); };
exports.rotateRight = function (l) { return l.rotateRight.bind(l); };
exports.subtract = function (l) { return l.subtract.bind(l); };
exports.toBytes = function (l) { return l.toBytes.bind(l); };
exports.toInt = function (l) { return l.toInt(); };
exports.toNumber = function (l) { return l.toNumber(); };
exports.toSigned = function (l) { return l.toSigned(); };
exports.toString = function (l) { return l.toString.bind(l); };
exports.toUnsigned = function (l) { return l.toUnsigned(); };
exports.xor = function (l) { return l.xor.bind(l); };
