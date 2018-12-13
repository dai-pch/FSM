package libpc.FSM

abstract class FSMException extends Exception {}

class MultipleEntryException extends FSMException {}

abstract class FSMCompileException extends FSMException {}

class FSMCompileNoStopping extends FSMCompileException {}