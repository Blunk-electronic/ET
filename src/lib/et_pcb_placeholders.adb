------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          PLACEHOLDERS ON THE PCB                         --
--                                                                          --
--                               B o d y                                    --
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


with ada.characters.handling;		use ada.characters.handling;
with et_board_coordinates;			use et_board_coordinates;


package body et_pcb_placeholders is
	


	function to_string (
		meaning : in type_text_meaning_conductor) 
		return string 
	is begin
		return type_text_meaning_conductor'image (meaning);
	end to_string;


	
	function to_meaning (
		meaning : in string) 
		return type_text_meaning_conductor 
	is begin
		return type_text_meaning_conductor'value (meaning);
	end to_meaning;



	function to_string (
		placeholder : in type_text_placeholder_conductors)
		return string
	is 
		tf : type_text_fab := type_text_fab (placeholder);
	begin
		return to_string (tf) 
			& " layer " & to_string (get_layer (placeholder))
			& " meaning " & to_string (get_meaning (placeholder));
	end to_string;


	

	function get_meaning (
		placeholder : in type_text_placeholder_conductors)
		return type_text_meaning_conductor
	is begin
		return placeholder.meaning;
	end get_meaning;

	

	function get_layer (
		placeholder : in type_text_placeholder_conductors)
		return type_signal_layer
	is begin
		return placeholder.layer;
	end get_layer;


	

	function to_string (
		placeholder : in pac_text_placeholders_conductors.cursor)					
		return string
	is begin
		return to_string (element (placeholder));
	end to_string;



	procedure iterate (
		placeholders	: in pac_text_placeholders_conductors.list;
		process			: not null access procedure (
							position : in pac_text_placeholders_conductors.cursor);
		proceed			: not null access boolean)
	is
		c : pac_text_placeholders_conductors.cursor := placeholders.first;
	begin
		while c /= pac_text_placeholders_conductors.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	
	
	function get_layer (
		placeholder : in pac_text_placeholders_conductors.cursor)					
		return type_signal_layer
	is begin
		return element (placeholder).layer;
	end get_layer;



	function is_selected (
		placeholder : in pac_text_placeholders_conductors.cursor)					
		return boolean
	is begin
		if is_selected (element (placeholder)) then
			return true;
		else
			return false;
		end if;
	end is_selected;
	


	function is_proposed (
		placeholder : in pac_text_placeholders_conductors.cursor)					
		return boolean
	is begin
		if is_proposed (element (placeholder)) then
			return true;
		else
			return false;
		end if;
	end is_proposed;



	
	function to_string (
		placeholder : in type_text_placeholder)
		return string
	is 
		tf : type_text_fab := type_text_fab (placeholder);
	begin
		return to_string (tf) 
			& " meaning " & to_string (get_meaning (placeholder));
	end to_string;



	function get_meaning (
		placeholder : in type_text_placeholder)
		return type_text_meaning
	is begin
		return placeholder.meaning;
	end get_meaning;

	


	procedure iterate (
		placeholders	: in pac_text_placeholders.list;
		process			: not null access procedure (
							position : in pac_text_placeholders.cursor);
		proceed			: not null access boolean)
	is
		c : pac_text_placeholders.cursor := placeholders.first;
	begin
		while c /= pac_text_placeholders.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;
	


	

	function to_string (
		placeholder : in pac_text_placeholders.cursor)
		return string
	is begin
		return to_string (element (placeholder));
	end to_string;

		

	

	

	function is_selected (
		placeholder : in pac_text_placeholders.cursor)					
		return boolean
	is begin
		if is_selected (element (placeholder)) then
			return true;
		else
			return false;
		end if;
	end is_selected;


	function is_proposed (
		placeholder : in pac_text_placeholders.cursor)					
		return boolean
	is begin
		if is_proposed (element (placeholder)) then
			return true;
		else
			return false;
		end if;
	end is_proposed;

	
	
end et_pcb_placeholders;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
