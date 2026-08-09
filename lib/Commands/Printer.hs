module Commands.Printer
  ( Printer,
    printLine,
    withStdoutPrinter,
    withSilentPrinter,
  )
where

import Bluefin.Compound qualified as Bluefin
import Bluefin.Eff ((:&), (:>))
import Bluefin.Eff qualified as Bluefin
import Bluefin.IO qualified as Bluefin

newtype Printer (es :: Bluefin.Effects) = MkPrinter
  {prPrintLineImpl :: forall e. Text -> Bluefin.Eff (e :& es) ()}

instance Bluefin.Handle Printer where
  mapHandle MkPrinter {..} =
    MkPrinter {prPrintLineImpl = Bluefin.useImplUnder . prPrintLineImpl}

printLine :: (e :> es) => Printer e -> Text -> Bluefin.Eff es ()
printLine pr = Bluefin.makeOp . prPrintLineImpl (Bluefin.mapHandle pr)

withStdoutPrinter ::
  (io :> es) =>
  Bluefin.IOE io ->
  (forall e. Printer e -> Bluefin.Eff (e :& es) r) ->
  Bluefin.Eff es r
withStdoutPrinter ioe action =
  Bluefin.useImplIn
    action
    MkPrinter {prPrintLineImpl = Bluefin.effIO ioe . putTextLn}

withSilentPrinter ::
  (forall e. Printer e -> Bluefin.Eff (e :& es) r) ->
  Bluefin.Eff es r
withSilentPrinter action =
  Bluefin.useImplIn
    action
    MkPrinter {prPrintLineImpl = const (pure ())}
