import 'dart:convert';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;

const apiBase = String.fromEnvironment(
  'API_BASE',
  defaultValue: 'http://localhost:8000',
);

void main() => runApp(const MyApp());

class MyApp extends StatelessWidget {
  const MyApp({super.key});
  @override
  Widget build(BuildContext context) =>
      MaterialApp(title: 'MRV GRK - Mobile', home: const Home());
}

class Home extends StatefulWidget {
  const Home({super.key});
  @override
  State<Home> createState() => _HomeState();
}

class _HomeState extends State<Home> {
  bool loading = false;
  String? err;
  Map<String, dynamic>? out;
  Future<void> hitung() async {
    setState(() {
      loading = true;
      err = null;
    });
    try {
      final res = await http.post(
        Uri.parse('$apiBase/calculate/total'),
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({
          "productionSystem": "Intensive",
          "animalCategory": "Adult Male",
          "LW": 248.41,
          "ADG": 0.339,
          "DMD_percent": 57.25,
          "DE_percent_for_IPCC": 54.4682,
          "CP_percent_diet": 7.3,
        }),
      );
      if (res.statusCode != 200) throw Exception('API ${res.statusCode}');
      setState(() => out = jsonDecode(res.body));
    } catch (e) {
      setState(() => err = e.toString());
    } finally {
      setState(() => loading = false);
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('MRV GRK - Mobile')),
      body: Padding(
        padding: const EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            ElevatedButton(
              onPressed: loading ? null : hitung,
              child: Text(loading ? 'Menghitung…' : 'Hitung Contoh'),
            ),
            if (err != null)
              Padding(
                padding: const EdgeInsets.only(top: 8),
                child: Text(err!, style: const TextStyle(color: Colors.red)),
              ),
            if (out != null) ...[
              const SizedBox(height: 12),
              Card(
                child: ListTile(
                  title: const Text('Total CO₂e (ton/th)'),
                  subtitle: Text('${out!['total']?['co2e_ton_year']}'),
                ),
              ),
              Card(
                child: ListTile(
                  title: const Text('Enteric CH₄ (kg/th)'),
                  subtitle: Text(
                    '${out!['enteric']?['ef_enteric_kg_ch4_year']}',
                  ),
                ),
              ),
              Card(
                child: ListTile(
                  title: const Text('Manure'),
                  subtitle: Text(
                    'CH₄: ${out!['manure']?['ch4_kg_year']} kg | N₂O: ${out!['manure']?['n2o_kg_year']} kg',
                  ),
                ),
              ),
            ],
          ],
        ),
      ),
    );
  }
}
