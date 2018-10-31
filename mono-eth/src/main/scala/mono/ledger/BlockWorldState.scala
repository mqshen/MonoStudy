package mono.ledger

import akka.util.ByteString
import mono.{Hash, UInt256, trie}
import mono.domain.{Account, Address, Blockchain, SignedTransaction}
import mono.ledger.BlockWorldState.AccountDelta
import mono.store.trienode.NodeKeyValueStorage
import mono.trie.MerklePatriciaTrie
import mono.vm.WorldState

object BlockWorldState {

  final class AccountDelta {
    private var _nonce = UInt256.Zero
    private var _balance = UInt256.Zero
    private var _stateRoot = Account.EmptyStorageRootHash
    private var _codeHash = Account.EmptyCodeHash

    def nonce = _nonce
    def balance = _balance
    def stateRoot = _stateRoot
    def codeHash = _codeHash

    def increaseNonce(nonce: UInt256) = {
      this._nonce += nonce
      this
    }

    def increaseBalance(balance: UInt256) = {
      this._balance += balance
      this
    }

    def withStateRoot(stateRoot: Hash) = {
      _stateRoot = stateRoot
      this
    }

    def withCodeHash(codeHash: Hash) = {
      _codeHash = codeHash
      this
    }

    override def toString = s"AccountDelta($nonce, $balance, $stateRoot, $codeHash)"
  }

  def apply(
             blockchain:         Blockchain,
             accountNodeStorage: NodeKeyValueStorage,
             storageNodeStorage: NodeKeyValueStorage,
             accountStartNonce:  UInt256,
             stateRootHash:      Option[Hash]        = None
           ): BlockWorldState = {
    val underlyingAccountsTrie = MerklePatriciaTrie[Address, Account](
      stateRootHash.getOrElse(Hash(trie.EmptyTrieHash)).bytes,
      accountNodeStorage
    )(Address.hashedAddressEncoder, Account.accountSerializer)

    new BlockWorldState(
      blockchain,
      accountNodeStorage,
      storageNodeStorage,
      accountStartNonce,
//      blockchain.evmCodeStorage,
      TrieAccounts(underlyingAccountsTrie),
      Map(),
      Set(),
      None
    )

  }
}
class BlockWorldState private (
  blockchain:                   Blockchain,
  accountNodeStorage:           NodeKeyValueStorage,
  storageNodeStorage:           NodeKeyValueStorage,
  accountStartNonce:            UInt256,
  private var trieAccounts:     TrieAccounts,
  private var accountDeltas:    Map[Address, Vector[BlockWorldState.AccountDelta]],
  private var touchedAddresses: Set[Address], // for debug
  private var stx:              Option[SignedTransaction] // for debug
) extends WorldState[BlockWorldState, TrieStorage] {

  /**
    * Returns world state root hash. This value is only updated/accessing after committed.
    */
  def stateRootHash: Hash = Hash(trieAccounts.underlying.rootHash)

  def getAccount(address: Address): Option[Account] = trieAccounts.get(address)

  def emptyAccount: Account = Account.empty(accountStartNonce)

  def saveAccount(address: Address, account: Account): BlockWorldState = {
    accountDeltas += (address -> (accountDeltas.getOrElse(address, Vector()) :+ toAccountDelta(address, account)))

    // should be added to trieAccounts after account delta calculated since toAccountDelta() is upon the original account
    trieAccounts += (address -> account)

    touchedAddresses += address

    // raceConditions on accounts have been considered during VM run, so do not need to addRaceCondition(OnAccount, address) here

    this
  }

  def saveCode(address: Address, code: ByteString): BlockWorldState = {
      println("need to implement saveCode")
//    codes += (address -> code)
//    addRaceCondition(OnCode, address)
//
//    trieAccounts += (address -> getGuaranteedAccount(address).withCodeHash(codeHash = Hash(crypto.kec256(code))))
//    addRaceCondition(OnAccount, address)

    this
  }

  def mergeRaceConditions(later: BlockWorldState): BlockWorldState = {
    println("need to implement mergeRaceConditions")
//    later.raceConditions foreach {
//      case (k, vs) => this.raceConditions += (k -> (this.raceConditions.getOrElse(k, Set()) ++ vs))
//    }
    this
  }

  /**
    * Used for debug
    */
  def withTx(stx: Option[SignedTransaction]) = {
    this.stx = stx
    this
  }

  private def toAccountDelta(address: Address, updatedAccount: Account): AccountDelta = {
    val Account(nonce, balance, stateRoot, codeHash) = getAccount(address).getOrElse(emptyAccount)
    val nonceDelta = updatedAccount.nonce - nonce
    val balanceDelta = updatedAccount.balance - balance
    new AccountDelta().increaseNonce(nonceDelta).increaseBalance(balanceDelta).withStateRoot(stateRoot).withCodeHash(codeHash)
  }

  /**
    * Updates state trie with current changes but does not persist them into the storages. To do so it:
    *   - Commits code (to get account's code hashes)
    *   - Commits constract storages (to get account's contract storage root)
    *   - Updates state tree
    *
    * @return Updated world
    */
  private[ledger] def commit(): BlockWorldState = {
    trieAccounts = trieAccounts.commit()
    this
  }

  /**
    * Should be called adter committed
    */
  def persist(): BlockWorldState = {
    // deduplicate codes first
    println("need to implement persist")
    /*
    this.codes.foldLeft(Map[Hash, ByteString]()) {
      case (acc, (address, code)) => acc + (Hash(crypto.kec256(code)) -> code)
    } foreach {
      case (hash, code) => evmCodeStorage + (hash -> code)
    }

    this.trieStorages.foreach {
      case (address, storageTrie) => storageTrie.underlying.persist()
    }

    this.trieAccounts.underlying.persist()
  */
    this
  }

  def deleteAccount(address: Address): BlockWorldState = {
    //TODO
    println("need to implement deleteAccount")
    trieAccounts -= address
//    trieStorages -= address
//    codes -= address

    touchedAddresses += address

//    addRaceCondition(OnAddress, address)
    this
  }

  def copy = new BlockWorldState(
    blockchain,
    accountNodeStorage,
    storageNodeStorage,
    accountStartNonce,
    trieAccounts,
    accountDeltas,
    touchedAddresses,
    stx
  )

  override def getCode(address: Address): ByteString = {
//    codes.get(address) match {
//      case Some(x) => x
//      case None =>
//        val code = getCodeFromEvmCodeStorage(address)
//        codes += (address -> code)
//        code
//    }
    println("need to implement get code")
    ByteString.empty
  }
}
