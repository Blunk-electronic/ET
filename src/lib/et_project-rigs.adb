------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PROJECT RIGS                                  --
--                                                                          --
--                               B o d y                                    --
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
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;

with ada.tags;

with ada.exceptions;
with ada.directories;

with et_general_rw;					use et_general_rw;
with et_time;						use et_time;


package body et_project.rigs is

	--use et_general.pac_net_name;

	function to_string (section : in type_section_name) return string is
	-- Converts a section like SEC_MODULE_INSTANCES to a string "module_instances".
		len : positive := type_section_name'image (section)'length;
	begin
		return to_lower (type_section_name'image (section) (5..len));
	end to_string;

	function compare_connectors (left, right : in type_connector) return boolean is
	-- Returns true if left connector comes before right connector.
	-- Returns false if connectors are equal.
		use pac_device_purpose;
		use pac_module_instance_name;
		r : boolean := false; -- to be returned
	begin
		-- First we compare instance_A
		if left.instance_A > right.instance_A then
			r := true;
		elsif left.instance_A < right.instance_A then
			r := false;
		else -- left instance_A equals right instance_A

			-- compare instance_B
			if left.instance_B > right.instance_B then
				r := true;
			elsif left.instance_B < right.instance_B then
				r := false;
			else -- left instance_B equals right instance_B

				-- compare purpose_A
				if left.purpose_A > right.purpose_A then
					r := true;
				elsif left.purpose_A < right.purpose_A then
					r := false;
				else -- left purpose_A equals right purpose_A

					-- compare purpose_B
					if left.purpose_B > right.purpose_B then
						r := true;
					elsif left.purpose_B < right.purpose_B then
						r := false;
					else 
						-- left purpose_B equals right purpose_B
						-- means: connectors are equal
						r := false;
					end if;
				end if;
			end if;
		end if;
		
		return r;
	end compare_connectors;
	
	procedure write_rig_configuration_header is 
		use et_general;
		use et_string_processing;
	begin
		-- write a nice header
		put_line (comment_mark & " " & system_name & " rig configuration file");
		put_line (comment_mark & " " & get_date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;
	end;

	procedure write_rig_configuration_footer is
		use et_string_processing;
	begin
		-- write a nice footer
		new_line;
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " " & get_date);
		put_line (comment_mark & " rig configuration file end");
		new_line;
	end;

	
	procedure save_rig_configuration (
		rig_cursor		: in pac_rigs.cursor;
		log_threshold 	: in type_log_level) 
		is separate;

	procedure read_rigs (
		log_threshold 	: in type_log_level)
		is separate;
		
end et_project.rigs;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
