------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          PLACEHOLDERS ON THE PCB                         --
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
with et_board_shapes_and_text;			use et_board_shapes_and_text;
with et_pcb_stack;						use et_pcb_stack;



package et_pcb_placeholders is
	
	use pac_text_board;


	
-- PLACEHOLDERS FOR TEXTS IN CONDUCTOR LAYERS:
	
	type type_text_meaning_conductor is (
		COMPANY,
		CUSTOMER,
		PARTCODE,
		DRAWING_NUMBER,
		ASSEMBLY_VARIANT,
		PROJECT, -- CS rename to PROJECT_NAME
		MODULE, -- CS rename to MODULE_NAME
		REVISION, -- CS rename to REVISION_NUMBER
		SIGNAL_LAYER_ID,
		SIGNAL_NAME
		);


	
	function to_string (
		meaning : in type_text_meaning_conductor) 
		return string;

	
	function to_meaning (
		meaning : in string) 
		return type_text_meaning_conductor;


	
	type type_text_placeholder_conductors is new 
		type_text_fab with 
	record
		meaning : type_text_meaning_conductor := type_text_meaning_conductor'first;

		-- the conductor layer the placeholder is placed in:
		layer	: type_signal_layer := type_signal_layer'first; 
	end record;

	
	-- There can be lots of placeholders of this kind. 
	-- So they can be are stored in a list:
	package pac_text_placeholders_conductors is new 
		doubly_linked_lists (type_text_placeholder_conductors);

	use pac_text_placeholders_conductors;

	

	-- Returns the signal layer of the given placeholder:
	function get_layer (
		placeholder : in pac_text_placeholders_conductors.cursor)					
		return type_signal_layer;
	

	function is_selected (
		placeholder : in pac_text_placeholders_conductors.cursor)					
		return boolean;


	function is_proposed (
		placeholder : in pac_text_placeholders_conductors.cursor)					
		return boolean;

	
	
-- PLACEHOLDERS FOR TEXTS IN NON-CONDUCTOR LAYERS:
		
	subtype type_text_meaning is type_text_meaning_conductor 
		range COMPANY .. REVISION;

	
	type type_text_placeholder is new
		type_text_fab with 
	record
		meaning : type_text_meaning := type_text_meaning'first;
	end record;

	
	package pac_text_placeholders is new 
		doubly_linked_lists (type_text_placeholder);

	use pac_text_placeholders;
		

	
	
	function is_selected (
		placeholder : in pac_text_placeholders.cursor)					
		return boolean;



	function is_proposed (
		placeholder : in pac_text_placeholders.cursor)					
		return boolean;

	
	
end et_pcb_placeholders;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
