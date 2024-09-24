------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             CANVAS TEXT                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2024                                                       --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

-- with ada.strings.bounded;
-- with ada.strings;
-- with ada.strings.fixed;
-- 

with et_text;					use et_text;


generic

	
package et_canvas.text is

	use cairo;
	
	procedure dummy;


	
	-- Draws a text on the canvas:
	procedure draw_text (
		content		: in et_text.pac_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font;
		anchor		: in type_vector_model; -- the anchor point in the model
		origin		: in boolean; -- when true, an origin is drawn at the anchor point
		rotation	: in type_rotation;
		alignment	: in et_text.type_text_alignment);


	
	-- Draw a vectorized text with the
	-- given linewidth:
	procedure draw_vector_text (
		text	: in pac_text.type_vector_text;
		width	: in pac_geometry.type_distance_positive);

	
	
end et_canvas.text;
