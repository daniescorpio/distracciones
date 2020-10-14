with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;

-- Packages needed to generate pulse interrupts       
-- with Ada.Interrupts.Names;
-- with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------

    -----------------------------------------------------------------------
    ------------- constants 
    -----------------------------------------------------------------------
    
    LEER_POSICION_CABEZA_PRIORITY 	  : Integer   := 1;
    LEER_DISTANCIA_PRIORITY 	  	  : Integer   := 2;
    MOSTRAR_INFO_DISPLAY_PRIORITY  	  : Integer   := 3;
    CALCULAR_RIESGOS_PRIORITY	  	  : Integer   := 4;
    LEER_GIRO_VOLANTE_PRIORITY 	  	  : Integer   := 5;
    INTERVALO_LECTURA_POSICION_CABEZA     : Time_Span := Milliseconds (400);
    INTERVALO_LECTURA_DISTANCIA_SEGURIDAD : Time_Span := Milliseconds (300);
    INTERVALO_LECTURA_GIRO_VOLANTE        : Time_Span := Milliseconds (350);
    INTERVALO_DETECCION_RIESGOS		  : Time_Span := Milliseconds (150);
    INTERVALO_REFRESCO_DISPLAY		  : Time_Span := Milliseconds (1000);
    
    
    -----------------------------------------------------------------------
    ------------- declaration of protected objects 
    -----------------------------------------------------------------------

    Protected Sintomas is
      procedure Distraccion (distraccionDetectada : out Boolean);
      procedure Volantazo;
    private
      Previous_H : HeadPosition_Samples_Type := (0, 0);
    end Sintomas;
    
    Protected Medidas is
      function LeerSensorCabeza return HeadPosition_Samples_Type;
    private
    end Medidas;
    
    -----------------------------------------------------------------------
    ------------- declaration of tasks 
    -----------------------------------------------------------------------

    -- Aqui se declaran las tareas que forman el STR

    task LeerDistancia is 
      pragma priority (LEER_DISTANCIA_PRIORITY);
    end LeerDistancia;
    
    task LeerPosicionCabeza is 
      pragma priority (LEER_POSICION_CABEZA_PRIORITY);
    end leerPosicionCabeza;
    
    task mostrarInfoDisplay is
      pragma priority (MOSTRAR_INFO_DISPLAY_PRIORITY);
    end; 
    
    task LeerGiroVolante is
      pragma priority (LEER_GIRO_VOLANTE_PRIORITY);
    end leerGiroVolante;


    -----------------------------------------------------------------------
    ------------- body of tasks 
    -----------------------------------------------------------------------


    -----------------------------------------------------------------------
    ------------- TAREA DISTANCIA DE SEGURIDAD 
    -----------------------------------------------------------------------

   task body LeerDistancia is
      	Current_D : Distance_Samples_Type := 0;
      	Current_V : Speed_Samples_Type := 0;
      	Siguiente_Instante   : Time;
	DS: Float:= 0.0;
 
   begin
	Siguiente_instante := Clock + INTERVALO_LECTURA_DISTANCIA_SEGURIDAD;
      	Starting_Notice ("Prueba sensor distancia");
	
	loop
      
        	Reading_Distance (Current_D);
        	Display_Distance (Current_D);
      
        	Reading_Speed (Current_V);
        	Display_Speed (Current_V);
		DS:= (Float(Current_V)/10.0)**2;
        
        	if (Float(Current_D) > DS) then
          		Light(On);

            	elsif(Float(Current_D) > DS/2.0) then
			Beep (4);
			Light(On);
		elsif(Float(Current_D) > DS/3.0 ) then
			Beep(5);
		end if;
		
		delay until Siguiente_Instante;
        	Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_DISTANCIA_SEGURIDAD;
        
        end loop;
        
      
    	Finishing_Notice ("Prueba sensor distancia");
      
    end leerDistancia;
    
    
    -----------------------------------------------------------------------
    ------------- TAREA POSICION CABEZA 
    -----------------------------------------------------------------------
    
    task body LeerPosicionCabeza is 
      DistraccionDetectada : Boolean;
      Siguiente_Instante   : Time;
      
    begin
    
      Siguiente_instante := Clock + INTERVALO_LECTURA_POSICION_CABEZA;
      Starting_Notice ("Prueba sensor cabeza");
      
      loop
      
        Sintomas.Distraccion(DistraccionDetectada);
        
        if (DistraccionDetectada = True) then 
          Beep(2);
          New_Line;
        end if;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_POSICION_CABEZA;
        
      end loop;
      
      Finishing_Notice ("Prueba sensor cabeza");
      
    end leerPosicionCabeza;
    
 
    -----------------------------------------------------------------------
    ------------- TAREA MOSTRAR INFO DISPLAY 
    -----------------------------------------------------------------------
        
    task body mostrarInfoDisplay is
      DistraccionDetectada : Boolean;
      Siguiente_Instante   : Time;
      
    begin
      
      Siguiente_instante := Clock + INTERVALO_REFRESCO_DISPLAY;
      Starting_Notice ("Prueba display");
      
      loop
        
        Sintomas.Distraccion(DistraccionDetectada);
        
        if (DistraccionDetectada = True) then
          Current_Time (Big_Bang);
   	  Put ("............%");
          Put_Line ("¡¡¡ DISTRACCION DETECTADA !!!");
        end if;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_REFRESCO_DISPLAY;
        
      end loop;
      
      Finishing_Notice ("Prueba display");
      
    end;
    
    -----------------------------------------------------------------------
    ------------- TAREA GIRO VOLANTE 
    -----------------------------------------------------------------------
    
    task body LeerGiroVolante is
    
      Current_S     	 : Steering_Samples_Type;
      Previous_S    	 : Steering_Samples_Type := 0;
      Actual_Offset 	 : Steering_Samples_Type;
      Siguiente_Instante : Time;
      
    begin
      
      Siguiente_instante := Clock + INTERVALO_LECTURA_GIRO_VOLANTE;
      Starting_Notice ("Prueba sensor giro volante");
      
      loop
        
        Reading_Steering (Current_S);
        
        Actual_Offset := Current_S - Previous_S;
        
        --if (Actual_Offset > 20 OR Actual_Offset < -20) then 
          --Sintoma de volantazo
          --Display_Steering (Current_S);
          --Put_Line (" --> VOLANTAZO");
        --end if;
        
        Previous_S := Current_S;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_GIRO_VOLANTE;
        
      end loop;
      
      Finishing_Notice ("Prueba sensor giro volante");
      
    end leerGiroVolante;
    
    
    -----------------------------------------------------------------------
    ------------- body of protected objects 
    -----------------------------------------------------------------------
    
    protected body Sintomas is
      
      procedure Distraccion (distraccionDetectada : out Boolean) is
        Current_H  : HeadPosition_Samples_Type;
        
      begin
      
        distraccionDetectada := False;
      
        Reading_HeadPosition(Current_H);
        
        if (Current_H(x) > 30 AND Previous_H(x) > 30) then 
          distraccionDetectada := True;
        end if;
        
        if (Current_H(x) < -30 AND Previous_H(x) < -30) then
          distraccionDetectada := True;
        end if;
        
        if (Current_H(y) > 30 AND Previous_H(y) > 30) then 
          distraccionDetectada := True;
        end if;
        
        if (Current_H(y) < -30 AND Previous_H(y) < -30) then
          distraccionDetectada := True;
        end if;
        
        Previous_H := Current_H;
        
      end Distraccion;
      
      
      procedure Volantazo is
        s : Steering_Samples_Type;
      begin
        Reading_Steering (s);
      end Volantazo;
      
    end Sintomas;
    
    
    protected body Medidas is
    
      function LeerSensorCabeza return HeadPosition_Samples_Type is
        h : HeadPosition_Samples_Type;
      begin
        Reading_HeadPosition(h);
        return h;
      end LeerSensorCabeza;
      
    end Medidas;
    
   
    ----------------------------------------------------------------------
    ------------- procedure para probar los dispositivos 
    ----------------------------------------------------------------------
    procedure Prueba_Dispositivos; 

    Procedure Prueba_Dispositivos is
        Current_V : Speed_Samples_Type := 0;
        Current_H : HeadPosition_Samples_Type := (+2,-2);
        Current_D : Distance_Samples_Type := 0;
        Current_O : Eyes_Samples_Type := (70,70);
        Current_E : EEG_Samples_Type := (1,1,1,1,1,1,1,1,1,1);
        Current_S : Steering_Samples_Type := 0;
    begin
         --Starting_Notice ("Prueba_Dispositivo");

         --for I in 1..120 loop
         -- Prueba distancia
            --Reading_Distance (Current_D);
            --Display_Distance (Current_D);
            --if (Current_D < 40) then Light (On); 
            --                    else Light (Off); end if;

         -- Prueba velocidad
            --Reading_Speed (Current_V);
            --Display_Speed (Current_V);
            --if (Current_V > 110) then Beep (2); end if;

         -- Prueba volante
            --Reading_Steering (Current_S);
            --Display_Steering (Current_S);
            --if (Current_S > 30) OR (Current_S < -30) then Light (On);
            --                                         else Light (Off); end if;

         -- Prueba Posicion de la cabeza
            --Reading_HeadPosition (Current_H);
            --Display_HeadPosition_Sample (Current_H);
            --if (Current_H(x) > 30) then Beep (4); end if;

         -- Prueba ojos
            --Reading_EyesImage (Current_O);
            --Display_Eyes_Sample (Current_O);

         -- Prueba electroencefalograma
            --Reading_Sensors (Current_E);
            --Display_Electrodes_Sample (Current_E);
   
         --delay until (Clock + To_time_Span(0.1));
         --end loop;

         --Finishing_Notice ("Prueba_Dispositivo");
         null;
    end Prueba_Dispositivos;


begin
   null;
   --Starting_Notice ("Programa Principal");
   --Prueba_Dispositivos;
   --Finishing_Notice ("Programa Principal");
end add;



