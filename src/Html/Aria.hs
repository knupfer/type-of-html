{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds    #-}

module Html.Aria where

import Html.Type

  -- [2020-11-03] ARIA https://w3c.github.io/aria/#states_and_properties
data instance Attribute "role"                        True False = RoleA

  -- 6.7 Definitios of States and Properties (all aria-* attributes)
data instance Attribute "aria-activedescendant"       True False = AriaActivedescendantA
data instance Attribute "aria-atomic"                 True False = AriaAtomicA
data instance Attribute "aria-autocomplete"           True False = AriaAutocompleteA
data instance Attribute "aria-braillelable"           True False = AriaBraillelableA
data instance Attribute "aria-brailleroledescription" True False = AriaBrailleroledescriptionA
data instance Attribute "aria-busy"                   True False = AriaBusyA
data instance Attribute "aria-checked"                True False = AriaCheckedA
data instance Attribute "aria-colcount"               True False = AriaColcountA
data instance Attribute "aria-colindex"               True False = AriaColindexA
data instance Attribute "aria-colindextext"           True False = AriaColindextextA
data instance Attribute "aria-colspan"                True False = AriaColspanA
data instance Attribute "aria-controls"               True False = AriaControlsA
data instance Attribute "aria-current"                True False = AriaCurrentA
data instance Attribute "aria-describedby"            True False = AriaDescribedbyA
data instance Attribute "aria-description"            True False = AriaDescriptionA
data instance Attribute "aria-details"                True False = AriaDetailsA
data instance Attribute "aria-disabled"               True False = AriaDisabledA
data instance Attribute "aria-dropeffect"             True False = AriaDropeffectA
data instance Attribute "aria-errormessage"           True False = AriaErrormessageA
data instance Attribute "aria-expanded"               True False = AriaExpandedA
data instance Attribute "aria-flowto"                 True False = AriaFlowtoA
data instance Attribute "aria-grabbed"                True False = AriaGrabbedA
data instance Attribute "aria-haspopup"               True False = AriaHaspopupA
data instance Attribute "aria-hidden"                 True False = AriaHiddenA
data instance Attribute "aria-invalid"                True False = AriaInvalidA
data instance Attribute "aria-keyshortcuts"           True False = AriaKeyshortcutsA
data instance Attribute "aria-label"                  True False = AriaLabelA
data instance Attribute "aria-labelledBy"             True False = AriaLabelledByA
data instance Attribute "aria-level"                  True False = AriaLevelA
data instance Attribute "aria-live"                   True False = AriaLiveA
data instance Attribute "aria-modal"                  True False = AriaModalA
data instance Attribute "aria-multiline"              True False = AriaMultilineA
data instance Attribute "aria-multiselectable"        True False = AriaMultiselectableA
data instance Attribute "aria-orientation"            True False = AriaOrientationA
data instance Attribute "aria-owns"                   True False = AriaOwnsA
data instance Attribute "aria-placeholder"            True False = AriaPlaceholderA
data instance Attribute "aria-posinset"               True False = AriaPosinsetA
data instance Attribute "aria-pressed"                True False = AriaPressedA
data instance Attribute "aria-readonly"               True False = AriaReadonlyA
data instance Attribute "aria-relevant"               True False = AriaRelevantA
data instance Attribute "aria-required"               True False = AriaRequiredA
data instance Attribute "aria-roledescription"        True False = AriaRoledescriptionA
data instance Attribute "aria-rowcount"               True False = AriaRowcountA
data instance Attribute "aria-rowindex"               True False = AriaRowindexA
data instance Attribute "aria-rowindextext"           True False = AriaRowindextextA
data instance Attribute "aria-rowspan"                True False = AriaRowspanA
data instance Attribute "aria-selected"               True False = AriaSelectedA
data instance Attribute "aria-setsize"                True False = AriaSetsizeA
data instance Attribute "aria-sort"                   True False = AriaSortA
data instance Attribute "aria-valuemax"               True False = AriaValuemaxA
data instance Attribute "aria-valuemin"               True False = AriaValueminA
data instance Attribute "aria-valuenow"               True False = AriaValuenowA
data instance Attribute "aria-valuetext"              True False = AriaValuetextA
