`timescale 1us/1ns
`include "axi_lite_template/agent_axi_lite.sv"
`include "fifo_if.sv"

module tb_axi_s_to_fifos;

    const integer t_clk    = 10;    // Clock period 100MHz

    localparam FIFO_DEPTH       = 32;
    localparam NUM_TRANSFERS    = FIFO_DEPTH * 100;
    localparam NUM_DRIVEN       = FIFO_DEPTH * 100;
    localparam DATA_WIDTH       = 32;

    localparam ADDR_STATUS  = 0;
    localparam ADDR_WRITE   = 1;
    localparam ADDR_READ    = 2;

    logic [31:0] data;

    logic [31:0] data_sent_axi[0:NUM_TRANSFERS-1] = '{default:0};
    logic [31:0] data_recv_axi[0:NUM_TRANSFERS-1] = '{default:0};
    logic [31:0] data_sent_fifo[0:NUM_TRANSFERS-1] = '{default:0};
    logic [31:0] data_recv_fifo[0:NUM_TRANSFERS-1] = '{default:0};

    int index_write_axi = 0;
    int index_read_axi  = 0;
    int index_write_fifo = 0;
    int index_read_fifo  = 0;

    axi_if #(
        .DATA_WIDTH(DATA_WIDTH),
        .ADDR_WIDTH(4)
    ) axi_if();

    fifo_if #(
        .DATA_WIDTH(DATA_WIDTH)
    ) fifo();
    
    // Conectar relojes y reset
    assign fifo.clk = axi_if.clk;
    assign fifo.rst = axi_if.rst;

    axi_lite_master #(
        .DATA_WIDTH(DATA_WIDTH),
        .ADDR_WIDTH(4)
    ) master;

    axi_s_to_fifos #(
        .C_DATA_WIDTH(DATA_WIDTH),
        .FIFO_DEPTH(FIFO_DEPTH)
    ) dut (
        .clk    (axi_if.clk),
        .rst    (axi_if.rst),

        // AXI4-Lite SLAVE
        .awaddr (axi_if.awaddr),
        .awprot (axi_if.awprot),
        .awvalid(axi_if.awvalid),
        .awready(axi_if.awready),

        .wdata  (axi_if.wdata),
        .wstrb  (axi_if.wstrb),
        .wvalid (axi_if.wvalid),
        .wready (axi_if.wready),

        .bresp  (axi_if.bresp),
        .bvalid (axi_if.bvalid),
        .bready (axi_if.bready),

        .araddr (axi_if.araddr),
        .arprot (axi_if.arprot),
        .arvalid(axi_if.arvalid),
        .arready(axi_if.arready),

        .rdata  (axi_if.rdata),
        .rresp  (axi_if.rresp),
        .rvalid (axi_if.rvalid),
        .rready (axi_if.rready),

        // FIFO write interface
        .fifo_wdata     (fifo.wdata),
        .fifo_full      (fifo.full),
        .fifo_almost_full(fifo.almost_full),
        .fifo_wena      (fifo.wena),

        // FIFO read interface
        .fifo_rdata     (fifo.rdata),
        .fifo_empty     (fifo.empty),
        .fifo_almost_empty(fifo.almost_empty),
        .fifo_rena      (fifo.rena)
    );

    task automatic full_fifo();
        logic [31:0] temp;
        master.read(temp, ADDR_STATUS * 4);
        full_fifo: assert (temp & 8)
            else $error("FIFO should be full(8) -> %d", temp);
        cov_full0: cover (1);
    endtask
    task automatic empty_fifo();
        logic [31:0] temp;
        master.read(temp, ADDR_STATUS * 4);
        empty_fifo: assert (temp & 2)
            else $error("FIFO should be empty(2) -> %d", temp);
        cov_empty0: cover (1);
    endtask
    task automatic almost_empty_fifo();
        logic [31:0] temp;
        master.read(temp, ADDR_STATUS * 4);
        almost_empty_fifo: assert (temp & 1)
            else $error("FIFO should be almost empty(1) -> %d", temp);
        cov_almost_empty1: cover (1);
    endtask
    task automatic almost_full_fifo();
        logic [31:0] temp;
        master.read(temp, ADDR_STATUS * 4);
        almost_full_fifo: assert (temp & 4)
            else $error("FIFO should be almost full(4) -> %d", temp);
        cov_almost_full1: cover (1);
    endtask

    task automatic check_status_consistency();
        logic [31:0] temp;
        bit full2, almost_full2, empty1, almost_empty1;

        master.read(temp, ADDR_STATUS * 4);
        full2         = temp[3];
        almost_full2  = temp[2];
        empty1        = temp[1];
        almost_empty1 = temp[0];

        assert (!(full2 && almost_full2))
            else $error("STATUS inconsistente: full2 && almost_full2");

        assert (!(empty1 && almost_empty1))
            else $error("STATUS inconsistente: empty1 && almost_empty1");
    endtask

    task check_size(int size, logic n_fifo);

        check_status_consistency();
        if (n_fifo) begin
            case (size)
                0: full_fifo();
                1: almost_full_fifo();
            endcase
        end else begin
            case (size)
                FIFO_DEPTH -1: almost_empty_fifo();
                FIFO_DEPTH: empty_fifo();
            endcase
        end

    endtask

    task automatic init_axi_write();
        for (int i=0; i<NUM_TRANSFERS; ++i) begin
            data_sent_axi[i] = i;
        end
    endtask

    task automatic init_fifo_write();
        for (int i=0; i<NUM_TRANSFERS; ++i) begin
            data_sent_fifo[i] = i;
        end
    endtask

    int index_axi_write = 0;
    task automatic axi_write_Ndata(input int N);
        for (int i=0; i<N; ++i) begin
            master.write(data_sent_axi[index_axi_write], ADDR_WRITE * 4, 4'hF);
            index_axi_write++;
        end
    endtask

    int index_fifo_write = 0;
    task automatic fifo_write_Ndata(input int N);
        for (int i=0; i<N; ++i) begin
            fifo.write_fifo(data_sent_fifo[index_fifo_write]);
            index_fifo_write++;
        end
    endtask

    int index_fifo_read = 0;
    task automatic fifo_read_Ndata(input int N);
        for (int i=0; i<N; ++i) begin
            fifo.read_fifo(data_recv_fifo[index_fifo_read]);
            index_fifo_read++;
        end
    endtask

    int index_axi_read = 0;
    task automatic axi_read_Ndata(input int N);
        for (int i=0; i<N; ++i) begin
            master.read(data_recv_axi[index_axi_read], ADDR_READ * 4);
            index_axi_read++;
        end
    endtask

    task check_integrity_sent_axi(int N);
        // Check the integrity of the data in the specified ports
        for (int i=0; i<N; ++i) begin
            assert (data_sent_axi[i] == data_recv_fifo[i])
                else $error("Data mismatch AXI write to FIFO read at index %0d: sent %0d, received %0d"
                    , i, data_sent_axi[i], data_recv_fifo[i]);
        end
    endtask

    task automatic empty_fifo_axi();
        logic empty = 0;
        logic [31:0] temp;
        while (!empty) begin
            master.read(temp, ADDR_READ * 4);
            master.read(temp, ADDR_STATUS * 4);
            empty = ^(temp & 2);
            @(posedge axi_if.clk);
        end
    endtask

    task check_integrity_sent_fifo(int N);
        // Check the integrity of the data in the specified ports
        for (int i=0; i<N; ++i) begin
            assert (data_sent_fifo[i] == data_recv_axi[i])
                else $error("Data mismatch FIFO write to AXI read at index %0d: sent %0d, received %0d"
                    , i, data_sent_fifo[i], data_recv_axi[i]);
        end
    endtask

    task automatic test_reset();
        for (int i = 0; i < FIFO_DEPTH; i++) begin
            master.write(i, ADDR_WRITE * 4, 4'hF);
            fifo.write_fifo(i);
        end
        @(posedge axi_if.clk);

        test_full_size_fifo1: assert (dut.size_fifo1 == 0)
            else $error("Assertion test_full_size_fifo1 failed! %0d", dut.size_fifo1);

        test_full_size_fifo2: assert (dut.size_fifo2 == 0)
            else $error("Assertion test_full_size_fifo2 failed! %0d", dut.size_fifo2);

        @(posedge axi_if.clk);
        axi_if.rst = 0;
        @(posedge axi_if.clk);
        axi_if.rst = 1;
        @(posedge axi_if.clk);

        test_reset_size_fifo1: assert (dut.size_fifo1 == FIFO_DEPTH)
            else $error("Assertion test_reset_size_fifo1 failed! %0d", dut.size_fifo1);

        test_reset_size_fifo2: assert (dut.size_fifo2 == FIFO_DEPTH)
            else $error("Assertion test_reset_size_fifo2 failed! %0d", dut.size_fifo2);

        empty_fifo();
        empty_fifo_axi();

    endtask

    task automatic test_overfill_under_axi_fifo();
        logic [31:0] temp_data;
        logic [31:0] status;
        int size_before, size_after;

        for (int i = 0; i < FIFO_DEPTH; i++) begin
            master.write(i, ADDR_WRITE * 4, 4'hF);
        end

        size_before = dut.size_fifo2;
        master.read(status, ADDR_STATUS * 4);
        assert(status[3])
            else $error("FIFO debería estar FULL tras %0d escrituras", FIFO_DEPTH);

        for (int i = 0; i < FIFO_DEPTH; i++) begin
            master.write(32'hDEADBEEF, ADDR_WRITE * 4, 4'hF);
            master.read(status, ADDR_STATUS * 4);
            assert(status[3])
                else $error("FIFO dejó de estar FULL durante overfill AXI");
            size_after = dut.size_fifo2;
            assert (size_before == size_after)
                else $error("FIFO debería estar FULL tras %0d escrituras", FIFO_DEPTH);
        end

        for (int i = 0; i < FIFO_DEPTH; i++) begin
            fifo.read_fifo(temp_data);
            assert (temp_data == i)
                else $error("Overfill AXI: esperaba %0d, leí %0d en posición %0d", 
                            i, temp_data, i);
        end

        @(posedge axi_if.clk);
        size_before = dut.size_fifo2;
        for (int i = 0; i < FIFO_DEPTH; i++) begin
            fifo.read_fifo(temp_data);
            assert (temp_data == 0) else 
                $error("Overread AXI: esperaba 0, leí %0d en posición %0d", temp_data, i);
            assert(fifo.empty) else
                $error("Tras sobreleer FIFO deberíamos seguir en estado vacío");
            size_after = dut.size_fifo2;
            assert (size_before == size_after)
                else $error("Leer ADDR_READ en vacío no debería cambiar size_fifo2");
        end
    endtask


    task test_overfill_under_fifo_fifo();
        logic [31:0] temp_data;
        logic [31:0] status;
        int size_before, size_after;

        for (int i = 0; i < FIFO_DEPTH; i++) begin
            fifo.write_fifo(i);
        end

        @(posedge axi_if.clk);
        assert(fifo.full) else $error("FIFO debería estar FULL tras escrituras %0d", FIFO_DEPTH);
        size_before = dut.size_fifo1;

        for (int i = 0; i < FIFO_DEPTH; i++) begin
            fifo.write_fifo(32'hDEADBEEF);
            assert(fifo.full) else $error("FIFO debería estar FULL tras escritura %0d", i);
            size_after = dut.size_fifo1;
            assert (size_before == size_after)
                else $error("FIFO debería estar FULL tras %0d escrituras", FIFO_DEPTH);
        end

        for (int i = 0; i < FIFO_DEPTH; i++) begin
            master.read(temp_data, ADDR_READ * 4);
            assert (temp_data == i)
                else begin
                    $error("Data mismatch AXI write to FIFO read at index %0d: sent %0d, received %0d"
                        , i, i, temp_data);
                end
        end

        master.read(status, ADDR_STATUS * 4);
        assert(status[1]) else
            $error("Tras leer todo el FIFO, debería estar vacío");
        size_before = dut.size_fifo1;

        for (int i = 0; i < FIFO_DEPTH; i++) begin
            master.read(temp_data, ADDR_READ * 4);
            assert (temp_data == 0)
                else begin
                    $error("Data mismatch on overread FIFO at index %0d: expected %0d, received %0d"
                        , i, 0, temp_data);
                end
            master.read(status, ADDR_STATUS * 4);
            assert(status[1]) else
                $error("Tras leer todo el FIFO, debería estar vacío");

            size_after = dut.size_fifo1;
            assert (size_before == size_after)
                else $error("Leer FIFO en vacío no debería cambiar size_fifo1");
        end
    endtask

    localparam ADDR_INVALID0 = 3;
    localparam ADDR_INVALID1 = 7;

    task automatic test_invalid_addresses();
        int size1_before, size1_after;
        int size2_before, size2_after;
        logic [31:0] d;

        size1_before = dut.size_fifo1;
        size2_before = dut.size_fifo2;

        master.write(32'hAAAA_BBBB, ADDR_INVALID0 * 4, 4'hF);
        master.write(32'hCCCC_DDDD, ADDR_INVALID1 * 4, 4'hF);

        master.read(d, ADDR_INVALID0 * 4);
        master.read(d, ADDR_INVALID1 * 4);

        size1_after = dut.size_fifo1;
        size2_after = dut.size_fifo2;

        assert(size1_before == size1_after)
            else $error("Acceso a direcciones inválidas ha modificado size_fifo1");
        assert(size2_before == size2_after)
            else $error("Acceso a direcciones inválidas ha modificado size_fifo2");

        check_status_consistency();
    endtask


    // check size assertions
    fifo1_size: assert property (@(posedge axi_if.clk) disable iff (!axi_if.rst)
        (dut.size_fifo1 <= FIFO_DEPTH))
        else $error("FIFO1 size exceeded depth");
    fifo2_size: assert property (@(posedge axi_if.clk) disable iff (!axi_if.rst)
        (dut.size_fifo2 <= FIFO_DEPTH))
        else $error("FIFO2 size exceeded depth");

    // Clock generation
    initial begin
        axi_if.clk = 0;
        forever #(t_clk/2) axi_if.clk = ~axi_if.clk;
    end

    // Reset generation and initialization
    task automatic init();
        axi_if.rst = 0;
        fifo.wena = 0;
        fifo.rena = 0;
        fifo.wdata = 0;
        init_axi_write();
        init_fifo_write();
        master = new(axi_if);
        #100 @(posedge axi_if.clk);
        axi_if.rst = 1;
        @(posedge axi_if.clk);
    endtask

    int remaining_axi_w  = NUM_TRANSFERS;
    int remaining_fifo_r = NUM_TRANSFERS;
    int remaining_fifo_w = NUM_TRANSFERS;
    int remaining_axi_r  = NUM_TRANSFERS;
    int unsigned free_space = FIFO_DEPTH;
    int unsigned occ;
    int unsigned data_send, data_recv;
    int max_send, max_recv;
    logic [DATA_WIDTH - 1:0] temp_data;
    typedef enum logic [2:0] {
        AXI_W_FIFO_R,
        FIFO_W_AXI_R,
        AXI_W1_FIFO_R1,
        FIFO_W1_AXI_R1,
        FIFO_1_EXTREME,
        FIFO_2_EXTREME
    } test_state; test_state state;

    initial begin
        init();
        @(posedge axi_if.clk);

        state = AXI_W_FIFO_R;
        free_space = FIFO_DEPTH;
        for (int step = 0; (remaining_axi_w > 0) || (remaining_fifo_r > 0); step++) begin

            occ = FIFO_DEPTH - free_space;
            max_send = (free_space < remaining_axi_w) ? free_space : remaining_axi_w;
            max_recv = (occ        < remaining_fifo_r) ? occ : remaining_fifo_r;
            data_send = (max_send == 0) ? 0 : $urandom_range(0, max_send);
            data_recv = (max_recv == 0) ? 0 : $urandom_range(0, max_recv);

            if (data_send > 0) begin
                axi_write_Ndata(data_send);
                free_space       -= data_send;
                remaining_axi_w  -= data_send;
            end

            if (data_recv > 0) begin
                fifo_read_Ndata(data_recv);
                free_space       += data_recv;
                remaining_fifo_r -= data_recv;
            end

            check_size(free_space, 1);
        end
        assert(index_axi_write  <= NUM_TRANSFERS);
        assert(index_fifo_read  <= NUM_TRANSFERS);
        check_integrity_sent_axi(NUM_TRANSFERS);

        state      = FIFO_W_AXI_R;
        free_space = FIFO_DEPTH;
        for (int step = 0; (remaining_fifo_w > 0) || (remaining_axi_r > 0); step++) begin

            occ = FIFO_DEPTH - free_space;
            max_send = (free_space < remaining_fifo_w) ? free_space : remaining_fifo_w;
            max_recv = (occ        < remaining_axi_r) ? occ : remaining_axi_r;
            data_send = (max_send == 0) ? 0 : $urandom_range(0, max_send);
            data_recv = (max_recv == 0) ? 0 : $urandom_range(0, max_recv);

            if (data_send > 0) begin
                fifo_write_Ndata(data_send);
                free_space      -= data_send;
                remaining_fifo_w -= data_send;
            end

            if (data_recv > 0) begin
                axi_read_Ndata(data_recv);
                free_space      += data_recv;
                remaining_axi_r -= data_recv;
            end

            check_size(free_space, 0);
        end
        assert(index_fifo_write <= NUM_TRANSFERS);
        assert(index_axi_read   <= NUM_TRANSFERS);
        check_integrity_sent_fifo(NUM_TRANSFERS);

        // driven test
        state = AXI_W1_FIFO_R1;
        fifo.empty_fifo();
        for (int i = 0; i < NUM_DRIVEN; i++) begin
            master.write(i, ADDR_WRITE * 4, 4'hF);
            fifo.read_fifo(temp_data);
            assert (temp_data == i)
                else begin
                    $error("Data mismatch AXI write to FIFO read at index %0d: sent %0d, received %0d"
                        , i, i, temp_data);
                end
        end

        state = FIFO_W1_AXI_R1;
        empty_fifo_axi();
        for (int i = 0; i < NUM_DRIVEN; i++) begin
            fifo.write_fifo(i);
            master.read(temp_data, ADDR_READ * 4);
            assert (temp_data == i)
                else begin 
                    $error("Data mismatch FIFO write to AXI read at index %0d: sent %0d, received %0d"
                    , i, i, temp_data);
                    $stop;
                end
        end

        state = FIFO_2_EXTREME;
        test_overfill_under_axi_fifo();
        test_overfill_under_axi_fifo();

        state = FIFO_1_EXTREME;
        test_overfill_under_fifo_fifo();
        test_overfill_under_fifo_fifo();

        test_invalid_addresses();

        test_reset();

        $display("All tests passed.");
        $stop;

    end

endmodule