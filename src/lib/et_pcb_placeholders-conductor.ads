------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   PLACEHOLDERS / PCB / CONDUCTOR LAYERS                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
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


with ada.containers; 					use ada.containers;
with ada.containers.doubly_linked_lists;
with et_board_text;						use et_board_text;
with et_pcb_signal_layers;				use et_pcb_signal_layers;



package et_pcb_placeholders.conductor is
	
	use pac_text_board;
	use pac_text_board_vectorized;
	


	
	type type_placeholder_conductor is new 
		type_text_fab with 
	record
		meaning : type_placeholder_meaning := placeholder_meaning_default;

		-- the conductor layer the placeholder is placed in:
		layer	: type_signal_layer := signal_layer_default; 
	end record;


	-- CS procedure reset_placeholder
	-- CS procedure set_meaning, set_layer
	-- CS function get_layer
	

	overriding function to_string (
		placeholder : in type_placeholder_conductor)
		return string;
	

	function get_meaning (
		placeholder : in type_placeholder_conductor)
		return type_placeholder_meaning;


	function get_layer (
		placeholder : in type_placeholder_conductor)
		return type_signal_layer;

						   
	-- There can be lots of placeholders of this kind. 
	-- So they can be are stored in a list:
	package pac_placeholders_conductor is new 
		doubly_linked_lists (type_placeholder_conductor);

	use pac_placeholders_conductor;


	-- Iterates the placeholders. 
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		placeholders	: in pac_placeholders_conductor.list;
		process			: not null access procedure (
							position : in pac_placeholders_conductor.cursor);
		proceed			: not null access boolean);
		
	
	function to_string (
		placeholder : in pac_placeholders_conductor.cursor)					
		return string;
	

	-- Returns the signal layer of the given placeholder:
	function get_layer (
		placeholder : in pac_placeholders_conductor.cursor)					
		return type_signal_layer;
	

	function is_selected (
		placeholder : in pac_placeholders_conductor.cursor)					
		return boolean;


	function is_proposed (
		placeholder : in pac_placeholders_conductor.cursor)					
		return boolean;


	
	
end et_pcb_placeholders.conductor;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
