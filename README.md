*******************************************  

## **Haskell Validator and Policie Scripts**


### **RATS DAP Developments** 

*******************************************  

Readme deprecated, needs update

*******************************************  

Dentro del Nix-Shell de la carpeta de Plutus Apps:  

Compilar con:  
> cabal build  

Tambien se pueden instalar para acceder a los ejecutables sin usar cabal.   
> cabal install  

*******************************************  
   
**Smart Contracts**  
  
Me gustaría empezar explicando un poquito cómo funcionan los Smart contracts.  
  
Los smart contracts se escriben en Haskell y es lo que llamamos la parte on chain.  
  
El código escrito en Haskell es convertido por medio del compilador de Haskell en código Plutus Script. Este código Plutus Script se reduce por medio de una función de hash a un hash y a una dirección dentro de la blockchain específica y única. Cualquier cambio en el código Haskell implica un nuevo hash y una nueva dirección. Para enviar ADA o Tokens a un contrato, o sea a la dirección qué obtuvimos a partir de su hash, es tan simple como cuando se envía ADA o Tokens de una wallet a otra wallet. La wallet que recibe no interviene en ninguna decisión. De la misma forma, el contrato, ni su lógica, intervienen a la hora de recibir.  
  
La lógica del contrato se ejecuta cuando se quiere consumir una uTxO que está en la dirección del contrato. Esto es similar a cuando yo quiero consumir una uTxO que está en alguna dirección de mi wallet, y para eso yo necesito la private key de esa wallet.  
  
El código Plutus Script del contrato es lo que sería la private Key necesaria para consumir una uTxO que esta en mi wallet, solo un código Haskell en particular, cuando es reducido a su hash, coincide con el hash de una dirección en particular.   
  
El código Plutus, que contiene la lógica del contrato, se envía a la blockchain en el momento en el que se quiere consumir una dirección de ese contrato. Se verificará primero que el hash de ese Plutus sea el que corresponde a esa dirección y luego se ejecutará el código Plutus recibido y de acuerdo a su salida, un boleano verdadero o falso, se aprobará el consumo de esa uTxO. Antes de la ultima actualización de Vasil, era necesario enviar el mismo contrato una y otra vez, cada vez que se deseaba consumir alguna uTxO de su dirección. A partir del fork Vasil es posible enviar el código una sola vez y luego en futuras tx sólo hacer referencia a la tx donde esta el código ya alojado en la Blockchain. Eso reduce considerablemente el tamaño de las tx que desean consumir uTxO de un contrato.  
  
**Valores**  
  
Cada uTxO (unspent transaction output), que son las salidas de las tx (transferencias), tienen un valor. Ese valor es una suma de ADA y tokens. Desde ahora lo llamaré el valor de una uTxO.  
  
**Datums**   
  
Hay una diferencia entre las uTxO que estan en la dirección de una wallet con las que estan en la dirección de un contrato. Las de los contratos tienen un dato adjunto y ese dato se creó de forma arbitraria en el momento en el que se envió ADA o tokens a la dirección del contrato.  Este dato se llama Datum y es donde se guarda información que el contrato tendrá en cuenta a la hora de validar el consumo de una uTxO y enviar sus valores a otra dirección en forma de una nueva uTxO.  
  
Por ej, en un contrato de locker por un tiempo definido hay un deadline en el Datum, una fecha que se determina a la hora de crear la transacción y se envía junto a los valores que se quieren bloquear en el contrato. Esta fecha será revisada por el contrato en el momento que alguien quiera consumir esa uTxO, para retirar los fondos del contrato, y aprobará o no la extracción.  

**Pasividad** 

Quizás ya hayan podido observar algo fundamental de los smart contract: el ser pasivo de los contratos.
Los contratos en sí no se ejecutan de por sí solos, si no sola y únicamente cuando alguien inicia una transferencia que quiere consumir una uTxO que está en un contrato, y su única función es aprobar o no está transacción.

**Inmutabilidad**

Otra cuestión importante a tener en cuenta en el funcionamiento de los contratos, los Datums y las uTxO, es que un Datum una vez creado no puede ser cambiado, es inmutable. Eso significa que sí yo tengo que actualizar un Datum necesito consumir la uTxO donde está el Datum que quiero actualizar y crear una nueva uTxO con el nuevo Datum. Por supuesto si habían ADA o Tokens en la uTxO que consumí, lógicamente tengo que crear la nueva uTxO con esa misma cantidad de ADA o Tokens. En cualquier caso, estas son algunas de las verificaciones básicas que tendrá que hacer el contrato a la hora de permitir la actualización de un Datum: que revise que el Datum consumido y el creado estén respectando cierta lógica del negocio y que los valores de las uTxO también se correspondan.

**Staking simple: centralizado en una sola uTxO**

Teniendo en cuenta lo antes mencionado, voy a definir primero un sistema de staking de la forma más simple posible. En la dirección del contrato hay una sola uTxO con un Datum que contiene toda la información del Pool, quien lo creó, a estos usuarios voy a llamarlos masters, su deadline, la fecha en la que termina el pool, el tipo de interés que paga, si cobra fees, y los usuarios registrados, junto a los fondos que los usuarios invirtieron y los rewards que fueron cobrando. Este sistema es unificado y toda la información importante esta en una única uTxO, cuyo valor es la suma de todos los fondos provistos por los masters, la inversión de todos los usuarios menos la suma de todos los rewards reclamados.

El primer paso es un usuario Master depositando fondos en el contrato. Para ello consume la uTxO con el Datum del Pool, enviá ADA o tokens desde su propia wallet y genera una salida nueva con un nuevo Datum, donde figura su fondo, y cuyo valor es el que agregó desde su wallet. Este valor quedará en esta uTxO disponible para el pago de rewards de los usuarios registrados. Al finalizar el Pool, el master podrá recupera los ADA o Tokens que no hayan sido reclamados por los usuarios en forma de rewards.

En el caso de registrar un nuevo usuario dentro del pool, tengo que consumir la uTxO con el Datum del Pool y crear una nueva cuyo Datum este actualizado, reflejando el nuevo usuario y su inversión. El valor en ADA o Tokens de la salida tendrá que ser igual al consumido, al anterior, más los nuevos fondos que el usuario agregó con su inversión desde su propia wallet.

Al querer obtener rewards, el usuario crea una tx que consume la uTxO con el Pool Datum, y crea una de salida con el Datum actualizado, reflejando el nuevo reclamo de rewards. En este caso, también habrá una salida hacia la wallet del usuario con el pago de los rewards. El monto final en la uTxO que queda en el contrato será igual al que había antes menos el pago de rewards efectuado.

Como ya lo mencionamos, cada vez que consumo esa uTxO para actualizar el Datum del tipo Pool, el contrato se ejecutará y validará ese consumo o no: para ello verificará que tipo de acción se esta haciendo y de acuerdo a ello que tipo de modificación permite en el Datum y como se ven afectados los valores de la uTxO, si deben ser mantenidos igual, si deben crecer (gracias a un nuevo fondo de un master o una nueva inversión de un usuario) o si deben bajar (gracias a un reclamo de rewards de un usuario). Solo aprobará aquella tx que este haciendo una acción correcta, una modificación correcta del Datum y de los valores de la uTxO.

**Problema de seguridad**

Como antes mencionamos, no hay ninguna validación ni lógica que se ejecute al agregar una uTxO en la dirección de un contrato. Entonces nada impide a un usuario mal intencionado crear una uTxO con un Datum similar al Datum válido del Pool, pero con alguna diferencia que le represente algún beneficio. Por ej, si los rewards de un usuario son en función de la fecha de su registro, o de la fecha de su último reclamo de rewards, este mismo podría crear un Datum que tenga una fecha anterior a la real en su registro, con el fin de que pueda solicitar más rewards de los que le tocarían y el contrato se los daría.

Entonces, cómo puede saber el contrato cúal uTxO tiene un Datum válido y cual no? Para ello se utilizan NFT.

**NFT**

La única forma que hay de verificar cuales uTxO son válidas y cuáles no es utilizando NFT. Por definición un NFT es único e irrepetible, entonces podemos estar seguro cuando buscamos una uTxO que aquella que tenga el NFT adecuado será la única valida. Si ese NFT acompaña a una uTxO con un Datum de tipo Pool, por ej, y yo deseo actualizar ese Datum, la salida tiene que ser otra uTxO, con el Datum actualizado y el mismo NFT.

El NFT debe ser minteado a la hora de crear la primera uTxO con ese tipo de Datum y debe ser quemado al consumir esa uTxO finalmente, en nuestro caso, cuando un Pool se cierra. Mientras el Pool esta activo ese NFT va pasando de uTxO a uTxO a medida que se actualice el Datum, señalando cual uTxO es la valida entre todas las uTxO que pueden haber en la dirección del contrato.

**Problemas de concurrencia**

El modelo de staking anterior puede funcionar a los efectos teóricos, pero en los prácticos puede sufrir un problema de concurrencia: si toda la información del Pool esta en una sola uTxO, cuyo Datum debe ser actualizado cada vez que hay alguna operación en el Pool, entonces es muy posible que dos o más usuarios quieran registrarse o obtener rewards en el mismo momento y sólo uno de ellos podrá consumir esa uTxO en ese momento. Sólo el primero en enviar la tx a la blockchain podrá ser validado. El resto generará txs que serán inválidas y no aprobadas a pesar de haber creado una tx valida en su momento. En el mejor de los casos tendrán que esperar a que se ejecute la tx anterior, que se genere una nueva uTxO con el Datum del Pool, y crear una nueva tx que va a consumir esa nueva uTxO. En el peor de los casos, sus fondos no se verán afectados, pero se verá imposibilitado de crear tx para registrarse u obtener rewards.

**Contratos parametrizados**

Quiero hacer también mención a otra posibilidad que permiten los contratos: a la hora de ejecutarse y validar una tx no sólo pueden acceder a los Datums si no pueden hacer uso de unos parámetros. Estos parámetros deben ser indicados a la hora de crear el contrato y serán hard-codeados en el código Plutus y van a ser también parte del hash y de la dirección del mismo. Eso significa que un mismo código de contrato, escrito en Haskell, que reciba diferentes parámetros, generará un código Plutus diferente, por ende, un hash y una dirección diferente también.

En nuestro caso, no es necesario cambiar el código Haskell del contrato de Staking cada vez que quiero modificar alguno de sus parámetros. Simplemente uso los nuevos parámetros, el mismo código, y obtengo una nueva dirección de contrato, que hará uso de esos nuevos parámetros indicados. Por ej cambiar la lista de Masters o el tipo de interes que paga el contrato.

**EndPoints**

Los endpoints de un contrato es lo que llamamos el código OffChain del contrato. Este código también es escrito en Haskell pero no es compilado ni convertido en Plutus Script ni tampoco se ejecuta en la blockchain. Es un código que se compila y se ejecuta en la computadora o server que brindará a los usuarios interacción con el contrato.

En muchos casos es el código que creará las txs que luego serán enviadas a la blockchain para que el contrato las valide. Sirve como interfaz para que diferentes frontends tengan un acceso común al contrato y transparente en cuanto al armado de la tx. Solamente debieran enviar parámetros y la parte OffChain se encargará de armar las txs correctas.

Es importante tener en cuenta que el codigo OffChain y el OnChain pueden tener código en común, eso significa que si el contrato OnChain usa una funcion para validar, por ejemplo la cantidad de rewards que un usuario puede obtener en un contrato de staking, la parte OffChain tambien puede acceder a esa misma función y armar txs que reclamen esa cantidad de rewards, calculados de la misma forma, asegurándome que la parte OnChain luego aprobará esa tx.

**Staking Plus: solución al problema de concurrencia**

En este caso no habrá una sola uTxO que necesite ser consumida una y otra vez por cada acción de los usuarios.
Tendré varias uTxO, y cada usuario podrá interactuar con alguna de ellas, dividiendo así el transito y el consumo.

Los usuarios del contrato también serán dos, los usuarios masters y los usuarios normales, que llamaré solamente masters y usuarios respectivamente. 

El contrato es del tipo parametrizado, lo que significa que debo crear primero los parametros del Pool, adjuntarlos al código Haskell y de allí obtener un código Plutus que hace uso de esos parámetros, y a partir de ese código obtengo un hash y  una dirección en particular. Recordemos que estos parámetros no pueden ser cambiados aposterior, quedan hard-codeados en el código del contrato que esta en esa dirección.

**Parámetros del Pool:**

```
{    
    ppPoolID  :: PoolID,   
    ppPoolIDTxOutRef :: LedgerApiV1.TxOutRef,   
    ppMasters :: Masters,   
    ppDeadline    :: Deadline,   
    ppInterest    :: Interest ,   
    ppMinumunInvest    :: Invest,   
    ppMaximunInvest    :: Invest,     
    ppFundIDPolicyId :: LedgerApiV1.CurrencySymbol,  
    ppNFTUserIDPolicyId :: LedgerApiV1.CurrencySymbol,  
    ppValidTimeRange :: LedgerApiV1.POSIXTime,   
    ppMinimunClaim :: Proffit   
}     
```

Algunas aclaraciones:  
**ppPoolID**: el NFT que identifica a este Pool.  
**ppMasters**: Masters del Pool: una lista de Payment Pub Key Hashes que determinan que wallets podrán tener funciones de privilegio sobre el Pool.  
**ppDeadline**: Fecha en que el Pool se termina.  
**ppInterest**: Para el calculo de rewards.  
**ppFundIDPolicyId**: Guardo la policy id para controlar si los NFT de nuevos FundDatum son correctos.  
**ppNFTUserIDPolicyId**: Guardo la policy id para controlar si los NFT de nuevos UserDatum son correctos.  

Tendremos además **tres tipos de Datums válidos** en las diferentes uTxO que estarán en la dirección del contrato: **Pool Datums, Fund Datums y User Datums**. 

**Habrá una única uTxO válida con un Datum del tipo Pool** y tendrá en su valor un NFT único llamado Pool ID. El contrato entonces podrá verificar la validez de una uTxO con un Pool Datum si y solo sí posee en su valor el NFT Pool ID que se corresponde con el Pool ID indicado en los parámetros del contrato. 

Si cambio el Pool ID de los parámetros del contrato, entonces tengo una nueva dirección de contrato. Por ende, en una única dirección de contrato hay un único Pool ID posible. Por eso puedo buscar ese Pool ID en los valores de las uTxOs y dar validez a la única uTxO que lo posea. De esta forma nadie puede crear en esa dirección del contrato una uTxO con un Pool Datum igual al válido. Sólo existe una sola vez un NFT y en este caso identifica a una única uTxO y nadie podrá suplantarlo.

**El único campo del Pool Datum es:**  

```
{  
    pdFundID_TNs :: [FundID]  
}  
```
Habrá en el contrato una única uTxO con un Pool Datum pero **podrán existir muchas uTxO con Fund Datums.**

La lista pdFundID_TNs de Fund IDs declarada arriba indicará la validez de los Fund Datums. Solo aquellas uTxOs con Fund Datums con alguno de los NFT Fund ID definidos en esta lista serán consideradas válidas.

La forma de corroborar la validez de esas uTxO, es que deben tener en su valor un NFT llamado Fund ID que será diferente y único para cada uno de ellas. Cada vez que quiero crear una nueva uTxO con un Fund Datum debo mintear un nuevo NFT del tipo Fund ID, debo crear un Fund Datum y debo actualizar el Pool Datum y agregar en la lista de Fund IDs al nuevo Fund ID.

Para actualizar el único Pool Datum que existirá en el contrato, debo consumir la uTxO que lo posee y es aquí donde la lógica del contrato es ejecutada. Aquí es donde me aseguro que la creación de un nuevo Fund Datum esta verificado por el contrato. El contrato es quien decide si todo esta correcto: si se esta creando un Fund Datum con los datos correctos, si este posee un NFT único llamado Fund ID, si se esta creando un nuevo Pool Datum actualizado: si se esta agregando en el Pool Datum viejo el nuevo Fund ID mencionado a la lista de Fund IDs y, por último, pero no menos importante, si ese NFT es minteado con una poliza de minteo que asegura que sea un NFT. Es para ello que se usa el campo ppFundIDPolicyId de los parámetros del Pool.

Si no fuera un NFT podría suceder que yo creo un Fund Datum válido, con un Fund ID, y este es registrado dentro del Pool Datum, dentro de la lista de Fund IDs. Luego podría venir un usuario mal intencionado y crear un nuevo Fund Datum, con valores que le benefician, y con el mismo Fund ID minteado y agregado. La única forma de evitar que esto suceda es asegurarme que el Fund ID minteado sea un NFT. La única forma de saber que es un NFT es saber con que política fué minteado. Por eso voy a crear una política para mintear NFT y la voy a declarar en los parámetros del Pool. Luego este en su código podrá controlar que el Fund ID sea un NFT minteado con esa política en particular. 

Como dijimos habrá entonces cuantas uTxOs con Fund Datums como necesite, serán validas siempre y cuando su NFT Fund ID esté en la lista de FundIDs del Pool Datum. Cada Fund Datum es un canal más que abre el ancho de banda de nuestro contrato. Los usuarios interactuarán con unos o con otros Fund Datums y podrán hacerlo en paralelo evitando los problemas de concurrencia antes mencionados. 
  
**Los campos del Fund Datum serán:**

```  
{
    fdFundID_TN :: FundID,  
    fdMasterFunders   :: [MasterFunder],  
    fdUserID_TNs :: [UserID],  
    fdCashedOut   :: Proffit  
}  
```
  
**fdFundID_TN**: El nombre del NFT que identifica a esta uTxO con este Fund Datum  
**fdMasterFunders**: Una lista de MastersFunders, que determina que Masters pusieron cuanto en el Pool como fondos para que los usuarios puedan cobrar sus rewards. Estos fondos estarán como valores en la uTxO con este Fund Datum.  
**fdUserID_TNs**: al igual que la lista que vimos antes en el Pool Datum, de Fund IDs, que es usada para validar las uTxO con Fund Datums, aquí hay una lista de User IDs que será usada para validar las uTxO con User Datums.  
**fdCashedOut**: Cuanto ha sido reclamado en forma de rewards de esta uTxO con Fund Datum.  
  
Para registrar un usuario en el Pool deberé actualizar algun Fund Datum de alguna uTxO del contrato. Deberé mintear un NFT User ID, agregarlo a la lista de User IDs del Fund Datum que estoy actualizado, y crear un User Datum donde registre los datos del usuario y su inversión. Nuevamente es aquí donde interviene la lógica del contrato. Se ejecutará por que se quiere consumir una uTxO que esta en su dirección. Se quiere consumir una uTxO con un Fund Datum por que se lo quiere actualizar y agregar el nuevo User ID a la lista. El contrato aprobará la tx sólo si, entre otras cosas: se esta teniendo como salida un User Datum valido: cuya fecha de registro es correcta, cuyo monto de inversion se corresponde a los valores que hay en la uTxO que viene de la wallet del usuario y que esos valores estan en esta uTxO de salida. Que esa uTxO de salida con User Datum tenga un NFT del tipo User ID minteado con la politica correcta. Que se este consumiento una uTxO del contrato con un Datum del tipo Fund Datum. Que se este generando una salida con un Datum del tipo Fund Datum. Que se este agregando en ese Datum el nuevo User ID. Que no se este modificando de ninguna manera los valores de la uTxO que poseía el Fund Datum.  
  
Si todo eso esta bien se creará una uTxO en el contrato con un User Datum válido. **Habrá tantas uTxO con User Datum como usuarios registrados dentro del Pool**. 

**Los campos del User Datum son**:
```
{ 
    udFundID :: FundID, 
    --en donde esta registrado 
    udUserID_TN :: UserID, 
    --id del registro, del userDatum que representa su registro 
    udUser :: User, 
    --pkh del user, para verificar claims 
    udInvest :: Invest, 
    udCreatedAt :: LedgerApiV1.POSIXTime, 
    udCashedOut :: Proffit, 
    udRewardsNotClaimed :: Proffit, 
    udLastClaimAt :: Maybe LedgerApiV1.POSIXTime 
}
```
Aquí es donde podemos ver lo importante que es saber cuales uTxOs son validas y cuales no. Si un usuario mal intencionado crease una uTxO en el contrato con un User Datum que diga que se registro hace dos años, podría luego obtener rewards por todo ese periodo y yo no tendría forma de verificar la valides de esas fechas. 
Cualquiera puede escribir Datums con cualquier forma en una dirección de un contrato.
Tengo yo que tener los mencanismos para saber diferenciar los validos de los que no.
Para eso se usan los NFT y esta cadena de vinculaciones que podría resumirse aquí:

Se define un NFT Pool ID y se asigna en los parámetros del contrato.
Los parámetros del contrato no pueden ser cambiados y junto al código Plutus determinan una única dirección.
En esa dirección hay un solo Pool ID posible y una y solo una uTxO podrá tener ese NFT en su valor.
Esa uTxO con el Pool ID contendrá el Pool Datum que llamaremos válido.
En ese Pool Datum estará la lista de los Fund IDs válidos.
Toda aquella uTxO con Fund ID válido contendrá un Fund Datum válido.
En esos Fund Datums se detalla la lista de User IDs válidos registrados.
Todas las uTxOs que tengan un User ID válido contendrán un User Datum válido.

De esta forma el contrato puede en cualquier momento saber si un User, Fund o Pool Datum es válido.

**EndPoints del contrato de Staking Plus**

**1. Master Crear Pool**:

El primer endpoint es el que va a crear el Pool Datum, en donde luego se podrán registrar Fund IDs y en ellos User IDs. Aquí el contrato no se ejecuta, pues no se esta consumiendo ninguna uTxO del contrato.

Se debe mintear el NFT Pool ID que se corresponde con los parámetros del contrato.
Se debe mintear un NFT Fund ID con la política correcta especificada en el contrato en ppFundIDPolicyId.
Se debe crear un Pool Datum con el Fund ID en la lista de Fund IDs.
Se debe crear un Fund Datum indicando el fondo de valores que el master deposita.

Se debe crear una tx cuyas entradas son:  
El minteo de Pool ID y Fund ID.  
Los valores del master para el fondo, que salen de su propia billetera.  
Y las salidas:  
Una uTxO con un Pool Datum y el NFT Pool ID.  
Una uTxO con un Fund Datum, el NFT Fund ID, y los valores del master para el fondo.  
  
**2. Master Fund Pool:**

Aquí se consume la uTxO con el Pool Datum para crear un nuevo Pool Datum con el nuevo Fund ID. Se crea ademas una nueva uTxO con un nuevo Fund Datum.
  
Se debe crear una tx cuyas entradas son:  
El minteo de un nuevo Fund ID.  
El consumo de la uTxO con Pool Datum válido y el NFT Pool ID.  
Los valores que el master disponga para el nuevo fondo, que salen de su propia billetera.  
Y las salidas son:  
Una uTxO con un Pool Datum actualizado, con el nuevo Fund ID registrado, y en su valor el NFT Pool ID.  
Una uTxO con un nuevo Fund Datum, el nuevo Fund ID, y los valores del master para el fondo.  
  
**3. Master Fund & Merge:**

Aquí se consume la uTxO con el Pool Datum válido, y se consumen una o varias uTxOs con Fund Datum válidos, para tener como salida una nueva uTxO con Pool Datum y una sóla nueva uTxO con Fund Datum. Sirve para unificar uTxO con Fund Datums.

Se debe crear una tx cuyas entradas son:  
El minteo de un nuevo Fund ID.  
El consumo de la uTxO con Pool Datum válido y el NFT Pool ID.  
Los valores que el master disponga para el nuevo fondo, que salen de su propia billetera.  
El consumo de una o varias uTxO con Fund Datums y sus respectivos Fund IDs.  
Y las salidas son:  
La quema de los Fund IDs que estoy uniendo.  
Una uTxO con un Pool Datum actualizado, con el nuevo Fund ID registrado, y en su valor el NFT Pool ID.  
Una uTxO con un nuevo Fund Datum, el nuevo Fund ID, los valores del master para el fondo y la suma de todos los valores que habia en las uTxOs con Fund Datums que estoy uniendo.  
  
**4. User Registro:**

Se consume alguna uTxO con Fund Datum y se obtiene de salida una nueva uTxO con un Fund Datum donde figure el User ID registrado en la lista de User IDs y una nueva uTxO con un User Datum junto al NFT User ID recien minteado. Los valores de esa uTxO con Fund Datum no deben ser tocados. El registro del usuario debe actualizar el Datum pero no modificar los valores de esa uTxO.

Se debe crear una tx cuyas entradas son:  
El minteo de un nuevo User ID.  
El consumo de la uTxO con Fund Datum y el NFT Fund ID.  
Los valores que el usuario disponga para la inversion, que salen de su propia billetera.  
Y las salidas son:  
Una uTxO con un Fund Datum actualizado, con el nuevo User ID registrado, y los valores y el NFT Fund ID que había antes presente en la uTxO con Fund Datum. Esos valores no se tocan. Solo se toca el Datum. Se actualiza.  
Una uTxO con un nuevo User Datum, cuyo valor contiene el nuevo NFT User ID y los valores que el usuario dispuso desde su billetera.  
  
Algo a prestar atención aquí: para evitar los problemas de concurrencia, al hacer el registro del usuario no es necesario consumir la uTxO con el Pool Datum. Solo alguna de las uTxO con Fund Datums. Y pueden haber muchas de ellas, por lo tanto, nuestro cuello de botella será tan chico o grande como querramos.

**5. User Obtener Rewards:**

Se consume la uTxO con Fund Datum donde el usuario esta registrado, y se obtiene de salida una nueva uTxO con un Fund Datum donde figure el nuevo monto reclamado. De esta uTxO que se consume se sacan los valores para pagar al usuario su reward. 

También se consume la uTxO donde esta el User Datum para reflejar la fecha de este reclamo y usar esa fecha en el futuro calculo de próximos rewards. Los valores de esa uTxO con el User Datum no deben ser modificados. La inversión del usuario pasa de la entrada a la salida sin tocarse.

Se debe crear una tx cuyas entradas son:  
El consumo de la uTxO con Fund Datum y el NFT Fund ID, y los valores disponibles para pagar rewards.  
La uTxO con el User Datum, que debe ser actualizado con el nuevo pago.  
Y las salidas son:  
Una uTxO con un Fund Datum actualizado, indicando los fondos reclamados, y cuyo valor sea el mismo que había antes menos lo reclamado.  
Una uTxO con User Datum actualizado, cuyo valor no se modifica.  
Una salida hacia la billetera del usuario con los rewards reclamados.  
  
Algo a prestar atención aquí: para evitar los problemas de concurrencia, al hacer los reclamos de rewards de los usuarios no es necesario consumir la uTxO con el Pool Datum. Solo la uTxO con Fund Datum donde figura el usuario registrado. Aquí surge la necesidad a tener en cuenta: si la uTxO con el Fund Datum donde el usuario esta registrado se queda sin fondos, tiene que ser posible que se adjunte otra uTxO con Fund Datum, donde el usuario no esta registrado, pero hay fondos, junto a la Fund Datum donde esta registrado, y con esas dos uTxOs el contrato puede validar el registro del usuario y pagar los rewards. Como se explicará más adelante, esto abre un hueco en la seguridad, y es por ello que en este tipo de tx, cuando se quiera usar más de una uTxO con Fund Datums, que se adjunte la uTxO con Pool Datum, para poder verificar las Fund Datums que estoy utilizando. Leer más adelante problemas y sus soluciones.

***6. User Invertir Rewards:**

Es una doble acción. Se reclaman rewards y se usan estos mismos para incrementar los fondos invertidos por el usuario. No se envian los rewards a la billetera del usuario si no que se envián de nuevo al contrato.  

**7. User Recuperar Inversion:**

El usuario recupera en su wallet los fondos que puso en la inversion. 

**8. Master Recuperar Fondos:**

El master recupera en su wallet un proporcional de lo que puso, de los fondos que no hayan sido usados para pagar los rewards.

**9. Master Close Pool:**

Se cierra el Pool. Se envia a todos los masters los fondos que le corresponden de lo que haya sobrado. Se envia a todos los usuarios sus inversiones.

**Problemas y sus soluciones**

Voy a extenderme un poco más en cómo pueden crearse uTxO con Datums falsos en la dirección del contrato y como se pueden validar los mismos.

Si en cada tx que creo, adjunto y consumo la uTxO con el Pool Datum, la uTxO con el Fund Datum y la uTxO con el User Datum, no habría problemas para validarlos a todos de manera sencilla y escalonada. La uTxO con Pool Datum es válida si tiene el NFT Pool ID que coincide con el Pool ID de los parámetros del contrato. La uTxO con Fund Datum es válida si tiene un NFT Fund ID presente en la lista de Fund IDs del Pool Datum validado anteriormente. Y la uTxO con User Datum es valida si tiene un NFT User ID registrado en la lista de User IDs del Fund Datum validado anteriormente. De esa forma, estoy seguro que estoy tratando con uTxOs con Datums validos.

Pero este no es siempre el caso. De hecho si así fuera, el problema de la concurrencia antes mencionado aparecería nuevamente: si toda acción del Pool debe consumir la uTxO con el Pool Datum, solamente una podrá consumirla en un momento dado. Es por eso que debo tener mecanismos para que el Pool pueda funcionar sin necesidad de consumir la uTxO con Pool Datum todo el tiempo.

Una solución que me parece posible es utilizar lo que se llama Inline Datum, otra funcionalidad nueva de la era Vasil, en donde al crear una tx se puede hacer referencia a un Datum existente, sin necesidad de consumir la uTxO donde esta presente. El contrato entonces podría tener acceso a ese Datum, en este caso a nuestro Pool Datum. En principio esto puede funcionar y quizás proveer alguna posibilidad de seguridad extra, pero no sería suficiente, ya que para poder validar ese Datum tengo que acceder al valor de esa uTxO y verificar que posea el NFT Pool ID necesario. Esto aún no lo he probado.

Entonces, como veníamos diciendo, si una tx quiere consumir solamente un Fund Datum y un User Datum, no puedo hacer su verificación, porque no tengo acceso al Pool Datum válido. Tampoco puedo saber si el Fund Datum es válido, aunque posea un NFT Fund ID minteado con la política correcta. Voy a explicar esto un poco más.

Una política de minteo puede ser utilizada por cualquier persona. Es un código que puede estar disponible. De hecho, debe estar disponible para que alguna auditoria pueda ver que es lo que hace y valida, y de esa forma asegurar que lo que produce son NFT, por ejemplo. Además nuestro código pretende ser open source, por lo tanto, la política que usemos podrá ser usada por cualquiera para mintear sus propios NFT. 

Para mintear el NFT Pool ID y los NFT de Fund IDs, se puede hacer que la política de minteo use una validación extra sobre que billeteras pueden mintear. Y en ese caso puedo limitar ese minteo a los Masters del Pool. De esa forma me aseguro que los masters del Pool solamente pueden hacer tx que minteen usando esa política. Eso se hace usando una política de minteado parametrizada.

Tal y como se explico con los Smart Contract, la misma política de minteado se define con parámetros específicos, en este caso una lista de Payment Pub Key Hashes de los masters, y estos parámetros se hard-codean en la política de minteado. La suma del código Haskell de la política de minteado más los parámetros generan una Policy ID (similar al hash de los Smart Contracts) que es única y responde a un código y parámetros específicos. Al querer mintear con esa Policy ID debo suministrar el código y los parámetros que generaron ese Hash, no puedo cambiarlos. Por lo tanto esa Policy ID verificará que quien la use sea uno de los Masters. Esto agrega un layer de seguridad, pero no es suficiente. Debemos también proteger a los masters de un master mal intencionado.

Pero para mitnear el NFT de User ID no puedo limitar esa política de minteado a algunas billeteras, pues, cualquier usuario debería ser capaz de crear una tx donde se registre en el Pool. Además recordemos que al registrarse el usuario debe suministrar valores desde su billetera, entonces solamente él puede hacerlo. 

Un master mal intencionado podría mintear un Fund ID nuevo, que no estará registrado dentro del Pool Datum válido. Podría luego mintear un User ID nuevo y crear un Fund Datum falso donde figure ese User ID registrado. Podría crear un User Datum falso con una fecha de registro falso y podría enviar esos Datums Falsos al contrato. Luego podría pretender ser el usuario beneficiado, y hacer un reclamo de rewards al contrato. Para ello enviaría al contrato el Fund Datum y el User Datum que creó y obtendría beneficios falsos en su billetera, pues el contrato no podría saber que estos Datums son falsos.

De todas formas, hasta aquí no hay problema, los fondos que recibiría salen de la uTxO con el Fund Datum falso que él mismo creó y fondeó. Se estaría pagando con su propio dinero. El problema surge cuando los fondos de esa uTxO se vacían poque existe un mecanismo para esos casos, que permite a los usuarios obtener rewards usando una uTxO con Fund Datum adicional donde haya fondos. En ese caso, usando la Fund Datum falsa y el User Datum Falso, podría obtener rewards de un Fund Datum válido.

Para evitar ese escenario, el mecanismo obliga a que en el momento que un usuario quiera reclamar fondos de una uTxO donde no esta registrado, que esa tx deba incluir la uTxO con el Pool Datum válido del Pool. Teniendo ese Pool Datum puedo verificar los Fund Datums que estoy recibiendo y controlar que sean válidos.

Otra cosa que puede suceder es que un usuario nuevo, distinto al mal intencionado, se registre en el Fund Datum falso. En ese caso podría obtener rewards de esa uTxO, afectando solamente al usuario mal intencionado, pero cuando los fondos de esa uTxO se vacíen, este no podrá reclamar fondos desde otra Fund Datum, puesto que para ello deberá incluir el Pool Datum y con él se verificará que su registro esta en un Fund Datum falso. Esto es una situación lamentable, pero se puede evitar si los medios por los que un usuario se registra son los canales oficiales. Nuestro FrontEnd, conectado con nuestro OffChain, se asegurará de registrar a los usuarios en Fund Datum válidos. Pero como no podemos evitar que los usuarios interactuen con nuestro contrato por otros canales, estos usuarios correrían el riesgo de registrarse en Fund Datums falsos. No corren riesgo de perder sus fondos, siempre tendrán mecanismos para reclamar su inversión, pero perderían la oportunidad de obtener los rewards que les corresponden.