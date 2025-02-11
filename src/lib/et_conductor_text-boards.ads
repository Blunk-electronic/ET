------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        CONDUCTOR TEXT BOARDS                             --
--                                                                          --
--                              S p e c                                     --
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



package et_conductor_text.boards is


	type type_conductor_text 
		is new et_conductor_text.type_conductor_text with
	record
		layer : type_signal_layer := type_signal_layer'first;
	end record;
	
	
	package pac_conductor_texts is new doubly_linked_lists (type_conductor_text);
	use pac_conductor_texts;


	-- Returns the given conductor text as string:
	function to_string (
		text : in pac_conductor_texts.cursor)
		return string;


	-- Returns true if the status flag "selected"
	-- of a text is set:
	function is_selected (
		text : in pac_conductor_texts.cursor)
		return boolean;


	-- Returns true if the status flag "proposed"
	-- of a text is set:
	function is_proposed (
		text : in pac_conductor_texts.cursor)
		return boolean;

	
	
	-- Iterates the texts. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		texts	: in pac_conductor_texts.list;
		process	: not null access procedure (position : in pac_conductor_texts.cursor);
		proceed	: not null access boolean);


	
	-- Logs the properties of the given text.
	procedure text_conductor_properties (
		cursor			: in pac_conductor_texts.cursor;
		log_threshold 	: in type_log_level);

	
end et_conductor_text.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
