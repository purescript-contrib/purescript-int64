exports.numberBitsToInt = function(x) {
  return x|0;
};

var Long = require("long");

var radixCheckers = {};

function RadixChecker(radix) {
  var digits;
  if (radix < 11) {
    digits = "[0-" + (radix - 1).toString() + "]";
  } else if (radix === 11) {
    digits = "[0-9a]";
  } else {
    digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
  }

  this.baseRE = new RegExp("^" + digits + "+$", "i");

  this.maxNegSignedBase = Long.MIN_VALUE.toString(radix).substring(1);
  this.maxPosSignedBase = Long.MAX_VALUE.toString(radix);
  this.maxUnsignedBase = Long.MAX_UNSIGNED_VALUE.toString(radix);
}

function hasValidDigits(isNegative, isUnsigned, base, radix) {
  radixCheckers[radix] = radixCheckers[radix] || new RadixChecker(radix);
  var rc = radixCheckers[radix];

  var maxBase;
  if (isUnsigned) {
    maxBase = rc.maxUnsignedBase;
  } else if (isNegative) {
    maxBase = rc.maxNegSignedBase;
  } else {
    maxBase = rc.maxPosSignedBase;
  }

  return ((base.length < maxBase.length)
          || (base.length === maxBase.length && base <= maxBase)
         ) && rc.baseRE.test(base);
}

exports._safeReadLong = function(s, isUnsigned, radix) {
  var isNegative = s.startsWith("-");
  var hasPositivePrefix = s.startsWith("+");

  var base = (isNegative || hasPositivePrefix) ? s.substring(1) : s;

  // remove preceding zeros
  var lastNdx = 0;
  while (lastNdx < base.length - 1 && base[lastNdx] === "0") { lastNdx++; }
  base = base.substring(lastNdx);

  var retVal = null;
  if (
    // long.js return 0 for empty string
    (base !== "")
    // long.js coerces negative values to their unsigned counterparts
      && (!isNegative || !isUnsigned || base === "0")
    // Invalid characters / overflow
      && hasValidDigits(isNegative, isUnsigned, base, radix)
  ) {
    try {
      var signPrefix = isNegative ? "-" : "";

      // long.js may throw errors
      retVal = Long.fromString(signPrefix + base, isUnsigned, radix);
    } catch (err) {
    }
  }

  return retVal;
};

exports.isWholeNumber = function(n) {
  return Number.isInteger(n);
};
