------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          CONDUCTOR TEXT                                  --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:

with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with et_mirroring;				use et_mirroring;
with et_pcb_sides;				use et_pcb_sides;
with et_board_geometry;			use et_board_geometry;
with et_board_text;				use et_board_text;
with et_design_rules_board;		use et_design_rules_board;
with et_text;					use et_text;
with et_pcb_signal_layers;		use et_pcb_signal_layers;
with et_logging;				use et_logging;


package et_conductor_text is

	use pac_geometry_2;

	
	use pac_geometry_brd;
	use pac_polygons;

	
	use pac_text_board;
	use pac_text_board_vectorized;




	type type_conductor_text 
		is new type_text_fab_with_content with
	record
		vectors	: type_vector_text; -- CS rename to text ?
	end record;


	
	-- Mirrors a text along the given axis:
	procedure mirror_text (
		text	: in out type_conductor_text;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);


	-- Rotates a text by the given angle about the origin:
	procedure rotate_text (
		text	: in out type_conductor_text;
		angle	: in type_rotation_model);


	-- Moves a text by the given offset:
	procedure move_text (
		text	: in out type_conductor_text;
		offset	: in type_vector_model);

		

	---- Logs the properties of the given text.
	--procedure text_conductor_properties (
		--cursor			: in pac_conductor_texts_board.cursor;
		--log_threshold 	: in type_log_level);

	
end et_conductor_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
