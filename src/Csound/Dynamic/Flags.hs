module Csound.Dynamic.Flags where

import Data.Default
import Text.PrettyPrint.Leijen

data Flags = Flags
    { audioFileOutput   :: AudioFileOutput
    , idTags            :: IdTags
    , rtaudio           :: Maybe Rtaudio
    , pulseAudio        :: Maybe PulseAudio
    , midiIO            :: MidiIO
    , midiRT            :: MidiRT
    , rtmidi            :: Maybe Rtmidi
    , displays          :: Displays
    , config            :: Config 
    , flagsVerbatim     :: Maybe String }

instance Default Flags where
    def = Flags def def def def def def def def def def

-- Audio file output

data AudioFileOutput = AudioFileOutput
    { formatSamples     :: Maybe FormatSamples
    , formatType        :: Maybe FormatType
    , output            :: Maybe String
    , input             :: Maybe String
    , nosound           :: Bool
    , nopeaks           :: Bool
    , dither            :: Maybe Dither }

instance Default AudioFileOutput where
    def = AudioFileOutput def def def def False False def
   
data FormatHeader = NoHeader | RewriteHeader

data FormatSamples 
    = Bit24 | Alaw | Uchar | Schar 
    | FloatSamples | Ulaw | Short | Long

data Dither = Triangular | Uniform

data FormatType 
    = Aiff | Au | Avr | Caf | Flac | Htk
    | Ircam | Mat4 | Mat5 | Nis | Paf | Pvf
    | Raw | Sd2 | Sds | Svx | Voc | W64 
    | Wav | Wavex | Xi

-- Output file id tags

data IdTags = IdTags 
    { idArtist      :: Maybe String
    , idComment     :: Maybe String
    , idCopyright   :: Maybe String
    , idDate        :: Maybe String
    , idSoftware    :: Maybe String
    , idTitle       :: Maybe String }

instance Default IdTags where
    def = IdTags def def def def def def

-- Realtime Audio Input/Output

data Rtaudio 
    = PortAudio | Alsa 
    | Jack 
        { jackClient    :: String
        , jackInport    :: String
        , jackOutport   :: String } 
    | Mme | CoreAudio
    | NoRtaudio

data PulseAudio = PulseAudio
    { paServer  :: String
    , paOutput  :: String
    , paInput   :: String }

-- MIDI File Input/Ouput

data MidiIO = MidiIO 
    { midiFile          :: Maybe String
    , midiOutFile       :: Maybe String
    , rawControllerMode :: Bool
    , midiSkipSeconds   :: Maybe Double
    , terminateOnMidi   :: Bool }

instance Default MidiIO where
    def = MidiIO def def False def False

-- MIDI Realtime Input/Ouput

data MidiRT = MidiRT
    { midiDevice        :: Maybe String
    , midiKey           :: Maybe Int
    , midiKeyCps        :: Maybe Int
    , midiKeyOct        :: Maybe Int
    , midiKeyPch        :: Maybe Int
    , midiVelocity      :: Maybe Int
    , midiVelocityAmp   :: Maybe Int
    , midiOutDevice     :: Maybe String }
   
instance Default MidiRT where
    def = MidiRT def def def def
                 def def def def

data Rtmidi = PortMidi | AlsaMidi | MmeMidi | WinmmMidi | NoRtmidi

-- Display

data Displays = Displays
    { csdLineNums       :: Maybe Int
    , displayMode       :: Maybe DisplayMode
    , displayHeartbeat  :: Maybe Int
    , messageLevel      :: Maybe Int
    , mAmps             :: Maybe Int
    , mRange            :: Maybe Int
    , mWarnings         :: Maybe Int
    , mDb               :: Maybe Int
    , mColours          :: Maybe Int
    , mBenchmarks       :: Maybe Int
    , msgColor          :: Bool
    , displayVerbose    :: Bool
    , listOpcodes       :: Maybe Int }

data DisplayMode = NoDisplay | PostScriptDisplay | AsciiDisplay

instance Default Displays where
    def = Displays def (Just NoDisplay) 
            def def def def
            def def def def
            False False
            def

-- Performance Configuration and Control

data Config = Config
    { hwBuf         :: Maybe Int
    , ioBuf         :: Maybe Int
    , newKr         :: Maybe Int
    , newSr         :: Maybe Int
    , scoreIn       :: Maybe String
    , omacro        :: Maybe (String, String)
    , smacro        :: Maybe (String, String)
    , setSched      :: Bool
    , schedNum      :: Maybe Int
    , strsetN       :: Maybe (Int, String)
    , skipSeconds   :: Maybe Double
    , setTempo      :: Maybe Int }

instance Default Config where
    def = Config def def def def def def def
                 False
                 def def def def   

----------------------------------------------------
-- rendering

-- just an alias for 'pretty'
p :: Pretty b => (a -> b) -> (a -> Doc)
p = (pretty . )

b :: String -> (a -> Bool) -> (a -> Doc)
b property extract a
    | extract a = text property
    | otherwise = empty    

fields :: a -> [a -> Doc] -> Doc
fields a fs = hsep $ fmap ( $ a) fs

instance Pretty Flags where
    pretty a = fields a 
        [ p audioFileOutput 
        , p idTags 
        , p rtaudio
        , p pulseAudio
        , p midiIO
        , p midiRT
        , p rtmidi
        , p displays
        , p config
        , p flagsVerbatim ]




data Flags = Flags
    { audioFileOutput   :: AudioFileOutput
    , idTags            :: IdTags
    , rtaudio           :: Maybe Rtaudio
    , pulseAudio        :: Maybe PulseAudio
    , midiIO            :: MidiIO
    , midiRT            :: MidiRT
    , rtmidi            :: Maybe Rtmidi
    , displays          :: Displays
    , config            :: Config 
    , flagsVerbatim     :: Maybe String }

