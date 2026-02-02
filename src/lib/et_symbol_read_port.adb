------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SYMBOL READ / PORT                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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
-- To Do:
-- - clean up
--
--



with ada.text_io;					use ada.text_io;
with ada.exceptions;

with et_schematic_geometry;			use et_schematic_geometry;

with et_logic;
with et_power_sources;
with et_port_sensitivity;
with et_port_strength;
with et_port_visibility;
with et_port_direction;
with et_port_names;
with et_symbol_ports;				use et_symbol_ports;
with et_keywords;					use et_keywords;


package body et_symbol_read_port is

	use pac_geometry_2;



	port					: type_port_base;
	port_name				: et_port_names.pac_port_name.bounded_string;
	port_direction			: et_port_direction.type_port_direction := et_port_direction.port_direction_default;
	port_sensitivity_edge	: et_port_sensitivity.type_sensitivity_edge := et_port_sensitivity.sensitivity_edge_default;
	port_sensitivity_level	: et_port_sensitivity.type_sensitivity_level := et_port_sensitivity.sensitivity_level_default;
	port_output_inverted	: et_logic.type_output_inverted := et_logic.output_inverted_default;
	port_output_tristate	: et_port_strength.type_output_tristate := et_port_strength.output_tristate_default;
	port_output_weakness	: et_port_strength.type_output_weakness := et_port_strength.output_weakness_default;
	port_power_level		: et_power_sources.type_power_level := et_power_sources.port_power_level_default;




	procedure read_port (
		line : in type_fields_of_line)
	is
		kw : string := f (line, 1);
		use et_port_visibility;
		use et_port_sensitivity;
		use et_port_strength;
		use et_logic;
		use et_power_sources;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 1 y 2
			expect_field_count (line, 5);

			-- extract the port position starting at field 2
			port.position := to_vector_model (line, 2);

		elsif kw = keyword_name then -- name I1A
			expect_field_count (line, 2);
			port_name := et_port_names.to_port_name (f (line, 2));

		elsif kw = keyword_length then -- length 5
			expect_field_count (line, 2);
			port.length := to_distance (f (line, 2));
			-- CS warning on zero length ?

		elsif kw = keyword_rotation then -- rotation 90.0
			expect_field_count (line, 2);
			port.rotation := to_rotation (f (line, 2));
			
		elsif kw = keyword_port_name_visible then -- port_name_visible yes/no
			expect_field_count (line, 2);
			port.port_name_visible := to_port_name_visible (f (line, 2));

		elsif kw = keyword_port_name_size then -- port_name_size 2.0
			expect_field_count (line, 2);
			port.port_name_size := to_distance (f (line, 2));

		elsif kw = keyword_terminal_name_visible then -- terminal_name_visible yes/no
			expect_field_count (line, 2);
			port.terminal_name_visible := to_terminal_name_visible (f (line, 2));

		elsif kw = keyword_terminal_name_size then -- terminal_name_size 2.0
			expect_field_count (line, 2);
			port.terminal_name_size := to_distance (f (line, 2));

		elsif kw = keyword_direction then -- direction BIDIR, PASSIVE, NOT_CONNECTED, ...
			expect_field_count (line, 2);
			port_direction := et_port_direction.to_port_direction (f (line, 2));

		elsif kw = keyword_sensitivity_edge then -- sensitivity_edge rising/falling/any
			expect_field_count (line, 2);
			port_sensitivity_edge := to_sensitivity_edge (f (line, 2));

		elsif kw = keyword_sensitivity_level then -- sensitivity_level high/low
			expect_field_count (line, 2);
			port_sensitivity_level := to_sensitivity_level (f (line, 2));

		elsif kw = keyword_inverted then -- inverted yes/no
			expect_field_count (line, 2);
			port_output_inverted := to_output_inverted (f (line, 2));

		elsif kw = keyword_tristate then -- tristate yes/no
			expect_field_count (line, 2);
			port_output_tristate := to_output_tristate (f (line, 2));

		elsif kw = keyword_level then -- level positive/negative/zero
			expect_field_count (line, 2);
			port_power_level := to_power_level (f (line, 2));

		elsif kw = keyword_weakness then -- weakness none/pull0/weak1 ...
			expect_field_count (line, 2);
			port_output_weakness := to_output_weakness (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_port;
	
		
		
		
		
		
	procedure insert_port (
		symbol			: in type_symbol_model_access;
		log_threshold	: in type_log_level)
	is		
		inserted	: boolean;
		cursor		: pac_symbol_ports.cursor;

		use et_port_names;
		use et_port_direction;
		use et_port_sensitivity;
		use et_port_strength;
		use et_logic;
		use et_power_sources;
	begin
		log (text => "insert port", level => log_threshold);
		log_indentation_up;
		
		
		case port_direction is
			when PASSIVE =>
				pac_symbol_ports.insert (
					container	=> symbol.ports,
					key			=> port_name,
					inserted	=> inserted,
					position	=> cursor,
					new_item	=> (port with 
						direction				=> PASSIVE)
					);

			when INPUT_ANALOG =>
				pac_symbol_ports.insert (
					container	=> symbol.ports,
					key			=> port_name,
					inserted	=> inserted,
					position	=> cursor,
					new_item	=> (port with 
						direction				=> INPUT_ANALOG)
					);

			when INPUT_DIGITAL =>
				pac_symbol_ports.insert (
					container	=> symbol.ports,
					key			=> port_name,
					inserted	=> inserted,
					position	=> cursor,
					new_item	=> (port with 
						direction				=> INPUT_DIGITAL,
						sensitivity_edge		=> port_sensitivity_edge,
						sensitivity_level		=> port_sensitivity_level)
					);

			when OUTPUT_ANALOG =>
				pac_symbol_ports.insert (
					container	=> symbol.ports,
					key			=> port_name,
					inserted	=> inserted,
					position	=> cursor,
					new_item	=> (port with 
						direction				=> OUTPUT_ANALOG,
						output_analog_tristate	=> port_output_tristate,
						output_analog_weakness	=> port_output_weakness)
					);

			when OUTPUT_DIGITAL =>
				pac_symbol_ports.insert (
					container	=> symbol.ports,
					key			=> port_name,
					inserted	=> inserted,
					position	=> cursor,
					new_item	=> (port with 
						direction				=> OUTPUT_DIGITAL,
						output_digital_inverted	=> port_output_inverted,
						output_digital_tristate	=> port_output_tristate,
						output_digital_weakness	=> port_output_weakness)
					);

			when BIDIR_DIGITAL =>
				pac_symbol_ports.insert (
					container	=> symbol.ports,
					key			=> port_name,
					inserted	=> inserted,
					position	=> cursor,
					new_item	=> (port with 
						direction				=> BIDIR_DIGITAL,
						output_inverted			=> port_output_inverted,
						output_tristate			=> port_output_tristate,
						output_weakness			=> port_output_weakness,
						input_sensitivity_edge	=> port_sensitivity_edge,
						input_sensitivity_level	=> port_sensitivity_level)
					);

			when POWER_OUT =>
				pac_symbol_ports.insert (
					container	=> symbol.ports,
					key			=> port_name,
					inserted	=> inserted,
					position	=> cursor,
					new_item	=> (port with 
						direction				=> POWER_OUT,
						level					=> port_power_level)
					);

			when POWER_IN =>
				pac_symbol_ports.insert (
					container	=> symbol.ports,
					key			=> port_name,
					inserted	=> inserted,
					position	=> cursor,
					new_item	=> (port with 
						direction				=> POWER_IN,
						level					=> port_power_level)
					);

			when NOT_CONNECTED =>
				pac_symbol_ports.insert (
					container	=> symbol.ports,
					key			=> port_name,
					inserted	=> inserted,
					position	=> cursor,
					new_item	=> (port with 
						direction				=> NOT_CONNECTED)
					);
		end case;

		
		-- abort if port name already used:
		if not inserted then
			log (ERROR, "port " & to_string (port_name) & " already in use !", console => true);
			raise constraint_error;
		end if;
		
		-- reset port parameters for next port
		port					:= (others => <>);
		port_name				:= to_port_name ("");
		port_direction			:= port_direction_default;
		port_sensitivity_edge	:= sensitivity_edge_default;
		port_sensitivity_level	:= sensitivity_level_default;
		port_output_inverted	:= output_inverted_default;
		port_output_tristate	:= output_tristate_default;
		port_output_weakness	:= output_weakness_default;
		port_power_level		:= port_power_level_default;

		log_indentation_down;
	end insert_port;
	

	
end et_symbol_read_port;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
