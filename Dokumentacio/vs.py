# -*- coding: utf-8 -*-
"""
    pygments.styles.vs
    ~~~~~~~~~~~~~~~~~~

    Simple style with MS Visual Studio colors.

    :copyright: Copyright 2006-2010 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Operator, Generic


class VisualStudioStyle(Style):

    background_color = "#ffffff"
    default_style = ""

    styles = {
        Comment:                   "#008000",
        Comment.Preproc:           "#0000ff",
        Keyword:                   "#0000ff",
        Operator.Word:             "#0000ff",
        Name.Tag:                  "#266BBD",
        Name.Attribute:            "#ee0000",
        Keyword.Type:              "#2b91af",        
        Name.Class:                "#2b91af",
        # String:                    "#a31515",
        String:                    "#3B7D25",        

        Generic.Heading:           "bold",
        Generic.Subheading:        "bold",
        Generic.Emph:              "italic",
        Generic.Strong:            "bold",
        Generic.Prompt:            "bold",

        Error:                     "border:#FF0000"
    }
