{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds    #-}

module Html.Aria
  ( module Html.Aria
  , module Html.Type
  ) where

import Html.Type

  -- [2020-11-03] ARIA https://w3c.github.io/aria/#states_and_properties
newtype instance Attribute "role"                        True v = RoleA v

  -- 6.7 Definitios of States and Properties (all aria-* attributes)
newtype instance Attribute "aria-activedescendant"       True v = AriaActivedescendantA v
newtype instance Attribute "aria-atomic"                 True v = AriaAtomicA v
newtype instance Attribute "aria-autocomplete"           True v = AriaAutocompleteA v
newtype instance Attribute "aria-braillelable"           True v = AriaBraillelableA v
newtype instance Attribute "aria-brailleroledescription" True v = AriaBrailleroledescriptionA v
newtype instance Attribute "aria-busy"                   True v = AriaBusyA v
newtype instance Attribute "aria-checked"                True v = AriaCheckedA v
newtype instance Attribute "aria-colcount"               True v = AriaColcountA v
newtype instance Attribute "aria-colindex"               True v = AriaColindexA v
newtype instance Attribute "aria-colindextext"           True v = AriaColindextextA v
newtype instance Attribute "aria-colspan"                True v = AriaColspanA v
newtype instance Attribute "aria-controls"               True v = AriaControlsA v
newtype instance Attribute "aria-current"                True v = AriaCurrentA v
newtype instance Attribute "aria-describedby"            True v = AriaDescribedbyA v
newtype instance Attribute "aria-description"            True v = AriaDescriptionA v
newtype instance Attribute "aria-details"                True v = AriaDetailsA v
newtype instance Attribute "aria-disabled"               True v = AriaDisabledA v
newtype instance Attribute "aria-dropeffect"             True v = AriaDropeffectA v
newtype instance Attribute "aria-errormessage"           True v = AriaErrormessageA v
newtype instance Attribute "aria-expanded"               True v = AriaExpandedA v
newtype instance Attribute "aria-flowto"                 True v = AriaFlowtoA v
newtype instance Attribute "aria-grabbed"                True v = AriaGrabbedA v
newtype instance Attribute "aria-haspopup"               True v = AriaHaspopupA v
newtype instance Attribute "aria-hidden"                 True v = AriaHiddenA v
newtype instance Attribute "aria-invalid"                True v = AriaInvalidA v
newtype instance Attribute "aria-keyshortcuts"           True v = AriaKeyshortcutsA v
newtype instance Attribute "aria-label"                  True v = AriaLabelA v
newtype instance Attribute "aria-labelledBy"             True v = AriaLabelledByA v
newtype instance Attribute "aria-level"                  True v = AriaLevelA v
newtype instance Attribute "aria-live"                   True v = AriaLiveA v
newtype instance Attribute "aria-modal"                  True v = AriaModalA v
newtype instance Attribute "aria-multiline"              True v = AriaMultilineA v
newtype instance Attribute "aria-multiselectable"        True v = AriaMultiselectableA v
newtype instance Attribute "aria-orientation"            True v = AriaOrientationA v
newtype instance Attribute "aria-owns"                   True v = AriaOwnsA v
newtype instance Attribute "aria-placeholder"            True v = AriaPlaceholderA v
newtype instance Attribute "aria-posinset"               True v = AriaPosinsetA v
newtype instance Attribute "aria-pressed"                True v = AriaPressedA v
newtype instance Attribute "aria-readonly"               True v = AriaReadonlyA v
newtype instance Attribute "aria-relevant"               True v = AriaRelevantA v
newtype instance Attribute "aria-required"               True v = AriaRequiredA v
newtype instance Attribute "aria-roledescription"        True v = AriaRoledescriptionA v
newtype instance Attribute "aria-rowcount"               True v = AriaRowcountA v
newtype instance Attribute "aria-rowindex"               True v = AriaRowindexA v
newtype instance Attribute "aria-rowindextext"           True v = AriaRowindextextA v
newtype instance Attribute "aria-rowspan"                True v = AriaRowspanA v
newtype instance Attribute "aria-selected"               True v = AriaSelectedA v
newtype instance Attribute "aria-setsize"                True v = AriaSetsizeA v
newtype instance Attribute "aria-sort"                   True v = AriaSortA v
newtype instance Attribute "aria-valuemax"               True v = AriaValuemaxA v
newtype instance Attribute "aria-valuemin"               True v = AriaValueminA v
newtype instance Attribute "aria-valuenow"               True v = AriaValuenowA v
newtype instance Attribute "aria-valuetext"              True v = AriaValuetextA v
