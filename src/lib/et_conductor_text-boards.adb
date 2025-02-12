------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        CONDUCTOR TEXT BOARDS                             --
--                                                                          --
--                              B o d y                                     --
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




package body et_conductor_text.boards is


	function to_string (
		text	: in pac_conductor_texts.cursor)
		return string
	is begin
		-- CS return position, layer, (content ?)
		return "";
	end to_string;



	function is_selected (
		text : in pac_conductor_texts.cursor)
		return boolean
	is 
		t : type_conductor_text := element (text);
	begin
		if is_selected (t) then
			return true;
		else
			return false;
		end if;
	end is_selected;
	


	function is_proposed (
		text : in pac_conductor_texts.cursor)
		return boolean
	is 
		t : type_conductor_text := element (text);
	begin
		if is_proposed (t) then
			return true;
		else
			return false;
		end if;
	end is_proposed;



	function get_layer (
		text : in pac_conductor_texts.cursor)
		return type_signal_layer
	is begin
		return element (text).layer;
	end get_layer;

	

	
	
	procedure iterate (
		texts	: in pac_conductor_texts.list;
		process	: not null access procedure (position : in pac_conductor_texts.cursor);
		proceed	: not null access boolean)
	is
		c : pac_conductor_texts.cursor := texts.first;
	begin
		while c /= pac_conductor_texts.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;
		


	
	procedure text_conductor_properties (
		cursor			: in pac_conductor_texts.cursor;
		log_threshold 	: in type_log_level) 
	is
		text : type_conductor_text;
	begin
		text := element (cursor);
		log (text => "conductor text signal layer" & to_string (text.layer) & " "
			& "content '" & et_text.to_string (text.content) & "'", level => log_threshold
			);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_conductor_properties;
	
	
end et_conductor_text.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
